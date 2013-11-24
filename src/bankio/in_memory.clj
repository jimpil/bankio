(ns bankio.in_memory
 (:require [clojure.test :refer [with-test is]]))
;------------------->Rudimemtary-logging-facilities<---------------------------------

;adopted from the book "Clojure Programming"

(def console  (agent *out*))
(def errors   (agent *err*))
(def accounts-log (agent (clojure.java.io/writer "logs/accounts.log" :append true)))
(def bank-log     (agent (clojure.java.io/writer "logs/bank.log"     :append true)))
(def denied-log   (agent (clojure.java.io/writer "logs/denied.log"   :append true)))  

(defn write [^java.io.Writer w & content]
 (doseq [x (interpose " " content)]
   (.write w (str x)))
   (doto w
     (.write "\n")
      .flush))      
      
(defn logged-ref [x & writer-agents]
  (add-watch (ref x) :log
     (fn [_ reference old new-state]
       (doseq [wr writer-agents]
         (send-off wr write new-state)))))

;------------------------------------------------------------------------------------
 
(defonce STARTING_AMOUNT 100)
(defonce OVERDRAFT_PENALTY 5)
(defonce OVERDRAFT_LIMIT -300)

(def rand-bank (ref (into {} (repeatedly 50000 
                          #(vector (keyword (gensym))
                            (ref (array-map
                                      :account-id (gensym)  ;;random unique id - constant amount
                                      :curr-balance STARTING_AMOUNT
                                      :overdraft {:limit   OVERDRAFT_LIMIT
                                                  :penalty OVERDRAFT_PENALTY})))) ))) 
                                                     
(defn- key-generator 
([seed]
 (let [i (atom seed)]
  (fn [] (-> i (swap! inc) str))))
([] 
 (key-generator 0)))    

; (repeatedly 10 generate-key) => ("1" "2" "3" "4" "5" "6" "7" "8" "9" "10") 
(defonce generate-key (key-generator)) ;;thread-safe unique-key generator 
                                     
(def bank1 ;;5 boys with ids 1-5
(logged-ref (zipmap (repeatedly 5 generate-key)
              (map logged-ref 
                     [{:name "Nick"   :curr-balance STARTING_AMOUNT :overdraft {:penalty OVERDRAFT_PENALTY :limit OVERDRAFT_LIMIT}}
                      {:name "John"   :curr-balance STARTING_AMOUNT :overdraft {:penalty OVERDRAFT_PENALTY :limit OVERDRAFT_LIMIT}}
                      {:name "Kostas" :curr-balance STARTING_AMOUNT :overdraft {:penalty OVERDRAFT_PENALTY :limit OVERDRAFT_LIMIT}}
                      {:name "Pierre" :curr-balance STARTING_AMOUNT :overdraft {:penalty OVERDRAFT_PENALTY :limit OVERDRAFT_LIMIT}}
                      {:name "Jim"    :curr-balance STARTING_AMOUNT :overdraft {:penalty OVERDRAFT_PENALTY :limit OVERDRAFT_LIMIT}}]
                (repeat accounts-log)))
   bank-log ))
                                                            
(def bank2 ;;5 girls with ids 6-10
(logged-ref (zipmap (repeatedly 5 generate-key) 
              (map logged-ref 
                     [{:name "Helen"  :curr-balance STARTING_AMOUNT   :overdraft {:penalty OVERDRAFT_PENALTY :limit OVERDRAFT_LIMIT}}
                      {:name "Mary"   :curr-balance STARTING_AMOUNT   :overdraft {:penalty OVERDRAFT_PENALTY :limit OVERDRAFT_LIMIT}}
                      {:name "Sophia" :curr-balance STARTING_AMOUNT   :overdraft {:penalty OVERDRAFT_PENALTY :limit OVERDRAFT_LIMIT}}
                      {:name "Lora"   :curr-balance STARTING_AMOUNT   :overdraft {:penalty OVERDRAFT_PENALTY :limit OVERDRAFT_LIMIT}}
                      {:name "Danielle" :curr-balance STARTING_AMOUNT :overdraft {:penalty OVERDRAFT_PENALTY :limit OVERDRAFT_LIMIT}}]))
    bank-log )) 

(defn credit "A low-level crediting function. Nothing to do with refs." 
 [account amount]
  (update-in account [:curr-balance] +' amount)) ;;safe math at the lowest level  
  
(defn- debit* 
"The actual debiting fn which adds an additional pernalty upon each overdraft. 
 Penalty amount depends on the customer-account." 
[account curr-balance debit-amount]
 (let [res (-' curr-balance debit-amount)]
  (if (neg? res)
    (if  (> (get-in account [:overdraft :limit]) res) 
      (throw (IllegalStateException. "Above your overdraft limit!")) 
      (-' res (get-in account [:overdraft :penalty])))
    res )))  
   
(defn debit "A low-level debiting function. Nothing to do with refs." 
 [account amount]
  (update-in account [:curr-balance] #(debit* account %1 %2) amount)) ;;safe math at the lowest level  
  
(defn deposit!
"Modifies the bank such that an amount is credited to this account.
 Returns the in-transaction value of the bank." 
[account amount]
 (dosync 
   (commute account credit amount)))
   
(defn withdraw! 
"Modifies the bank such that an amount is debited from this account.
 Returns the in-transaction value of the bank." 
[account amount]
 (dosync 
   (alter account debit amount)))                         

(defn- transfer1 
"Modifies bank such that an amount is tranfered from an account 'from' to account 'to'.
 Will throw if used on its own. Prefer 'transfer!' instead." 
 [amount from to]
   (alter to credit amount)
   (alter from debit amount))
         
(with-test         
(defn transfer! 
"Entry point for transactions. Accepts an arbitrary number of [amount from to] triplets. 
 Uses 'transfer1' per each triplet. Returns nil." 
 [& amount-from-to]
 (assert (zero? (rem (count amount-from-to) 3)) "Need mulitples of 3 [amount from to]")
  (dosync
    (doseq [[amount from to] (partition 3 amount-from-to)]
      (try  (transfer1 amount from to) 
      (catch IllegalStateException e
        (send-off denied-log write {:account @from :amount amount})
        (send-off errors write "[DENIED-OVERDRAFT]:" @from ":>" amount)))))) 
;---------------------------------------------------------            
(let [dummy (atom [(ref {:id "0001" :curr-balance 56 :overdraft {:penalty OVERDRAFT_PENALTY :limit OVERDRAFT_LIMIT}}) 
                   (ref {:id "0002" :curr-balance 89 :overdraft {:penalty OVERDRAFT_PENALTY :limit OVERDRAFT_LIMIT}})
                   (ref {:id "0003" :curr-balance 0  :overdraft {:penalty OVERDRAFT_PENALTY :limit OVERDRAFT_LIMIT}})])
      _     (transfer! 11 (get @dummy 0) (get @dummy 1) 
                       25 (get @dummy 1) (get @dummy 2)
                       20 (get @dummy 2) (get @dummy 0))]       
(is (= @(get @dummy 0)  {:id "0001" :curr-balance 65 :overdraft {:penalty OVERDRAFT_PENALTY :limit OVERDRAFT_LIMIT}}))
(is (= @(get @dummy 1)  {:id "0002" :curr-balance 75 :overdraft {:penalty OVERDRAFT_PENALTY :limit OVERDRAFT_LIMIT}}))
(is (= @(get @dummy 2)  {:id "0003" :curr-balance 5  :overdraft {:penalty OVERDRAFT_PENALTY :limit OVERDRAFT_LIMIT}}))) )

(defn balance 
 "Returns the balance of the this particular account." 
 [account] 
  (get-in account [:curr-balance]))
  
(defn- interest 
 "A higher-order function responsible for calculating valid interest-rates (0-1 inclusive). 
  Returns a fn which expects an amount to apply the rate to which in turn will return the amount to be added (see apply-interest)." 
[rate]
 (assert (and (>= 1 rate) 
              (<= 0 rate)) "Cannot accept an interest-rate less than 0 or greater than 1.")
 (fn [amount]
   (*' amount rate)))
   
(defn- apply-interest
"Returns a new account with interest applied to its current balance.
 Typically, 'rate-fn' is constructed via 'interest' ."
[account rate-fn]
  (credit account (rate-fn (balance account))))  ;or simply  (* rate (balance account)) 

(with-test       
(defn apply-interest!
"Modifies bank such that the same interest-rate is applied to the balances of the accounts corresponding to ids.
 Returns nil." 
[rate & accounts]
(let [rate-fn (interest rate)]
 (dosync 
   (doseq [acc accounts]
     (alter acc apply-interest rate-fn))))) 
;-------------------------------------------------------------------    
(let [dummy (atom [(ref {:id "0001" :curr-balance 60 :overdraft {:penalty OVERDRAFT_PENALTY :limit OVERDRAFT_LIMIT}}) 
                   (ref {:id "0002" :curr-balance 90 :overdraft {:penalty OVERDRAFT_PENALTY :limit OVERDRAFT_LIMIT}})
                   (ref {:id "0003" :curr-balance 10 :overdraft {:penalty OVERDRAFT_PENALTY :limit OVERDRAFT_LIMIT}})] )
      _     (apply-interest! 0.5 (get @dummy 0) (get @dummy 1) (get @dummy 2))] ;same interest to all three             
(is (= @(get @dummy 0)  {:id "0001" :curr-balance 90.0  :overdraft {:penalty OVERDRAFT_PENALTY :limit OVERDRAFT_LIMIT}}))
(is (= @(get @dummy 1)  {:id "0002" :curr-balance 135.0 :overdraft {:penalty OVERDRAFT_PENALTY :limit OVERDRAFT_LIMIT}}))
(is (= @(get @dummy 2)  {:id "0003" :curr-balance 15.0  :overdraft {:penalty OVERDRAFT_PENALTY :limit OVERDRAFT_LIMIT}}))) )

     
(defn new-account 
"Returns a new bank with [id acc-map] added or throws if the id already exists.
 Does not affect the original bank." 
[bank id acc-map]
(if-not (contains? bank id) ;;necessary check
  (assoc bank id (logged-ref acc-map accounts-log))
  (throw (IllegalStateException. "ID already exists!")))) ;;should never throw if keys are unique
    
(defn- delete-account 
"Returns a new bank with these id deleted or throws if the balance is not zero. Does not affect the original bank." 
([bank id protect-ref?]
(let [deref-fn (if protect-ref? ensure deref)] 
 (if (->> id (get bank) deref-fn :curr-balance zero?) ;;delete an account only if its balance is zero
   (dissoc bank id)
   (throw (IllegalStateException. "Non-zero balance!")))))
([bank id]
  (delete-account bank id false)) )                                                        
       
(defn open-accounts! 
"Entry point for opening accounts. 
 Expects a bank and a series of ids,accounts such that (partition 2 ids-accs) leaves nothing behind.
 Returns the in-transaction value of the bank." 
 [bank & ids-accs] 
 (assert (zero? (rem (count ids-accs) 2)) "Need mulitples of 2 [id account-map]")
  (dosync
   (doseq [[id acc] (partition 2 ids-accs)]
    (try (commute bank new-account id acc)
    (catch IllegalStateException e 
      (send-off errors write "[DENIED-OPENING]:" acc ":>" id)))) @bank))
     
(defn close-accounts!
"Entry point for closing accounts. 
 Expects a bank and any number of account ids to be deleted.
 Returns the in-transaction value of the bank." 
[bank & ids]
 (dosync  
  (doseq [id ids]
    (try (alter bank delete-account id true)
    (catch IllegalStateException e 
     (send-off errors write "[DENIED-CLOSING]:" @(get bank id))))) @bank))
     
(defn total 
 "Sum all accounts. It expects the bank map - not the ref."
 [bank]
  (reduce +'
    (map (comp :curr-balance deref second) bank)))
    
(defn- stress-test ;;use bank1 or bank2 and poke the atom (i.e. ask for total or open a new accounts) while this fn is running
([bank nthreads sleep-time]
 (dotimes [i nthreads]
   (when (zero? (rem i 10))
   (future (close-accounts! bank (str (/ i 2))))
   (let [k1 (generate-key)
         k2 (generate-key)] 
     (open-accounts! bank k1 {:name (str "BRAND-NEW" k1) :curr-balance STARTING_AMOUNT 
                              :overdraft {:penalty OVERDRAFT_PENALTY :limit OVERDRAFT_LIMIT}} 
                          k2 {:name (str "BRAND-NEW" k2) :curr-balance STARTING_AMOUNT
                              :overdraft {:penalty OVERDRAFT_PENALTY :limit OVERDRAFT_LIMIT}})))
  (future
   (Thread/sleep (rand-int sleep-time)) ;;sleep up to sleep-time
   (transfer!   (rand-int STARTING_AMOUNT)  ;;random amount 0-99
                (get @bank (-> bank deref count inc rand-int str)) ;;pick accounts randomly   
                (get @bank (-> bank deref count inc rand-int str))
                (rand-int (* 2 STARTING_AMOUNT))  ;;random amount 0-49
                (get @bank (-> bank deref count inc rand-int str)) ;;pick accounts randomly
                (get @bank (-> bank deref count inc rand-int str))))))
([bank threads] 
  (stress-test bank threads 5000))
([bank] 
  (stress-test bank STARTING_AMOUNT))  )
  
;(open-accounts! bank1 "111" {:name "BRAND-NEW1" :curr-balance STARTING_AMOUNT} "222" {:name "BRAND-NEW2" :curr-balance STARTING_AMOUNT})  
    
