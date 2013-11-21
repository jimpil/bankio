(ns bankio.core
 (:require [clojure.test :refer [with-test is]]))
 
;(defrecord Customer [id name])

(def rand-bank (atom (into {} (repeatedly 50000 
                          #(vector (keyword (gensym))
                            (ref (array-map
                                      :account-id (gensym)  ;;random unique id - constant amount
                                      :curr-balance 100)))) )))

(def bank-keys (atom (map str (range)))) ;;infinite string keys (i.e "1" "2" "3" etc)
(defn generate-key  []
  (let [k (first @bank-keys)]
    (swap! bank-keys next) k))
                                     
(def bank1 ;;5 boys with ids 1-5
(atom  (zipmap (repeatedly 5 generate-key)
            (map ref [{:name "Nick" :curr-balance 100}
                      {:name "John" :curr-balance 100}
                      {:name "Kostas" :curr-balance 100}
                      {:name "Pierre" :curr-balance 100}
                      {:name "Jim" :curr-balance 100}]))))
                                                            
(def bank2 ;;5 girls with ids 6-10
(atom  (zipmap (repeatedly 5 generate-key) 
            (map ref [{:name "Helen" :curr-balance 100}
                      {:name "Mary" :curr-balance 100}
                      {:name "Sophia" :curr-balance 100}
                      {:name "Lora" :curr-balance 100}
                      {:name "Danielle" :curr-balance 100}])))) 

(defn credit "A low-level crediting function. Nothing to do with refs." 
 [account amount]
  (update-in account [:curr-balance] +' amount)) ;;safe math at the lowest level
   
(defn debit "A low-level debiting function. Nothing to do with refs." 
 [account amount]
  (update-in account [:curr-balance] -' amount)) ;;safe math at the lowest level
  
(defn deposit!
"Modifies the bank such that an amount is credited to this account.
 Returns the modified bank." 
[account amount]
 (dosync 
   (commute account credit amount)))
   
(defn withdraw! 
"Modifies the bank such that an amount is debited from this account.
 Returns the modified bank." 
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
"Entry point for transactions. Accepts a bank to transact on and an arbitrary number of [amount from to] triplets. 
 Uses 'transfer1' per each triplet." 
 [& amount-from-to]
 (assert (zero? (rem (count amount-from-to) 3)) "Need mulitples of 3 [amount from to]")
  (dosync
    (doseq [[amount from to] (partition 3 amount-from-to)]  
      (transfer1 amount from to)))) 
;---------------------------------------------------------            
(let [dummy (atom [(ref {:id "0001" :curr-balance 56}) 
                   (ref {:id "0002" :curr-balance 89})
                   (ref {:id "0003" :curr-balance 0})])
      _     (transfer! 11 (get @dummy 0) (get @dummy 1) 
                       25 (get @dummy 1) (get @dummy 2)
                       20 (get @dummy 2) (get @dummy 0))]       
(is (= @(get @dummy 0)  {:id "0001" :curr-balance 65}))
(is (= @(get @dummy 1)  {:id "0002" :curr-balance 75}))
(is (= @(get @dummy 2)  {:id "0003" :curr-balance 5}))) )

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
 Returns the in-transaction value of the bank." 
[rate & accounts]
(let [rate-fn (interest rate)]
 (dosync 
   (doseq [acc accounts]
     (alter acc apply-interest rate-fn))))) 
;-------------------------------------------------------------------    
(let [dummy (atom [(ref {:id "0001" :curr-balance 60}) 
                   (ref {:id "0002" :curr-balance 90})
                   (ref {:id "0003" :curr-balance 10})] )
      _     (apply-interest! 0.5 (get @dummy 0) (get @dummy 1) (get @dummy 2))] ;same interest to all three             
(is (= @(get @dummy 0)  {:id "0001" :curr-balance 90.0}))
(is (= @(get @dummy 1)  {:id "0002" :curr-balance 135.0}))
(is (= @(get @dummy 2)  {:id "0003" :curr-balance 15.0}))) )

     
(defn new-account 
"Returns a new bank with [id acc-map] added or nil if the id already exists.
 Does not affect the original bank." 
[bank id acc-map]
(when-not (contains? bank id) ;;necessary check
  (assoc bank id (ref acc-map))))
    
(defn delete-accounts 
"Returns a new bank with these ids deleted. Does not affect the original bank." 
[bank & ids]
  (apply dissoc bank ids))                                                     
       
(defn open-accounts! 
"Entry point for opening accounts. 
 Expects a bank and a series of ids,accounts such that (partition 2 ids-accs) leaves nothing behind.
 Returns the modified bank." 
 [bank & ids-accs] 
 (assert (zero? (rem (count ids-accs) 2)) "Need mulitples of 2 [id account-map]")
   (doseq [[id acc] (partition 2 ids-accs)]
     (swap! bank new-account id acc)) @bank)
     
(defn close-accounts!
"Entry point for closing accounts. 
 Expects a bank and any number of account ids to be deleted.
 Returns the modified bank." 
[bank & ids]
  (swap! bank delete-accounts ids))
     
(defn total 
 "Sum all accounts. It expects the bank map - not the ref."
 [bank]
  (reduce +'
    (map (comp :curr-balance deref second) bank)))
    
(defn- stress-test ;;use bank1 or bank2 and poke the atom (i.e. ask for total or open a new accounts) while this fn is running
([bank nthreads sleep-time]
 (dotimes [i nthreads]
   (when (zero? (rem i 10)) ;;every 10 transactions we open 2 new accounts so for 100 times we should be expecting 2500 money at the end
   (let [k1 (generate-key)
         k2 (generate-key)] 
     (open-accounts! bank k1 {:name (str "BRAND-NEW" k1) :curr-balance 100} 
                          k2 {:name (str "BRAND-NEW" k2) :curr-balance 100})))
  (future
   (Thread/sleep (rand-int sleep-time)) ;;sleep up to sleep-time
   (transfer!   (rand-int 100)  ;;random amount 0-99
                (get @bank (-> bank deref count inc rand-int str)) ;;pick accounts randomly   
                (get @bank (-> bank deref count inc rand-int str))
                (rand-int 50)  ;;random amount 0-49
                (get @bank (-> bank deref count inc rand-int str)) ;;pick accounts randomly
                (get @bank (-> bank deref count inc rand-int str))))))
([bank threads] 
  (stress-test bank threads 5000))
([bank] 
  (stress-test bank 100))  )
  
;(open-accounts! bank1 "111" {:name "BRAND-NEW1" :curr-balance 100} "222" {:name "BRAND-NEW2" :curr-balance 100})  
    
