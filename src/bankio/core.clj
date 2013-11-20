(ns bankio.core
 (:require [clojure.test :refer [with-test is]]))

;STM 'Bank' canonical example. Correct math & safe transactional semantics have priority.

(defrecord Customer [id name])

(def bank (ref (into {} (repeatedly 50000 
                                      #(vector (keyword (gensym))
                                        (array-map
                                         :account-id (gensym)  ;;random unique id - constant amount
                                         :curr-balance 100))) )))
                                         
;(def watched-bank (ref (mapv #(add-watch %...) @bank)))                                          
                                          
;(def account1 (get @bank 15))
;(def account2 (get @bank 100))

#_(def dubank (ref [{:id "0001" :curr-balance 56} 
                   {:id "0002" :curr-balance 89}
                   {:id "0003" :curr-balance 0}]))              
                  
(defn credit "A low-level crediting function. Nothing to do with refs." 
 [bank acc-id amount]
  (update-in bank [acc-id :curr-balance] +' amount)) ;;safe math at the lowest level
   
(defn debit "A low-level debiting function. Nothing to do with refs." 
 [bank acc-id amount]
  (update-in bank [acc-id :curr-balance] -' amount)) ;;safe math at the lowest level
  
(defn deposit!
"Modifies the bank such that an amount is credited to this account-id.
 Returns the modified bank." 
[bank acc-id amount]
 (dosync 
   (alter bank credit acc-id amount)))
   
(defn withdraw! 
"Modifies the bank such that an amount is debited from this account-id.
 Returns the modified bank." 
[bank acc-id amount]
 (dosync 
   (alter bank debit acc-id amount)))                         

(defn- transfer1 
"Modifies bank such that an amount is safely tranfered from an account-id 'from' to an account-id 'to'.
 Returns the modified bank." 
 [bank amount from to]
 (dosync
  (alter bank 
    (fn [b]
     (-> b
       (credit to amount)
       (debit  from amount))))))
         
(with-test         
(defn transfer! 
"Entry point for transactions. Accepts a bank to transact on and an arbitrary number of [amount from to] triplets. 
 Uses 'transfer1' per each triplet. Returns the modified bank." 
 [bank & amount-from-to]
 (assert (zero? (rem (count amount-from-to) 3)) "Need mulitples of 3 [amount from to]")
  (dosync
    (doseq [[amount from to] (partition 3 amount-from-to)]  
      (transfer1 bank amount from to))) @bank)
       
(let [dummy (ref [{:id "0001" :curr-balance 56} 
                  {:id "0002" :curr-balance 89}
                  {:id "0003" :curr-balance 0}])]       
(is (= (transfer! dummy 11 0 1 
                        25 1 2 
                        20 2 0) 
      [{:id "0001" :curr-balance 65} 
       {:id "0002" :curr-balance 75}
       {:id "0003" :curr-balance 5}]))) )

(defn balance 
 "Returns the balance of the this particular account-id." 
 [bank acc-id] 
  (get-in bank [acc-id :curr-balance]))
  
(defn- interest 
 "A higher-order function responsible for calculating valid interest-rates (0-1 inclusive). 
  Returns a fn which expects an amount to be updated according to the rate." 
[rate]
 (assert (and (>= 1 rate) 
              (<= 0 rate)) "Cannot accept an interest-rate less than 0 or greater than 1.")
 (fn [amount]
   (*' amount rate)))
   
(defn apply-interest
"Returns a new bank with interest applied to the account id"
[bank id rate]
  (credit bank id ((interest rate) (balance bank id))))  ;or simply  (* rate (balance bank id)) 

(with-test       
(defn apply-interest!
"Modifies bank such that the same interest-rate is applied to the balances of the accounts corresponding to ids.
 Returns the modified bank." 
[bank rate & ids]
 (dosync 
   (doseq [id ids]
     (alter bank apply-interest id rate))) @bank) 
    
(let [dummy (ref [{:id "0001" :curr-balance 60} 
                  {:id "0002" :curr-balance 90}
                  {:id "0003" :curr-balance 10}])] 
 (is (= (apply-interest! dummy 0.5 0 1) 
    [{:id "0001" :curr-balance 90.0} 
     {:id "0002" :curr-balance 135.0}
     {:id "0003" :curr-balance 10}]))
 (is (= (apply-interest! dummy 0.25 2) 
    [{:id "0001" :curr-balance 90.0} 
     {:id "0002" :curr-balance 135.0}
     {:id "0003" :curr-balance 12.5}]))))
     
(defn new-account 
"Returns a new bank with [id acc-map] added or nil if the id already exists.
 Does not affect the original bank." 
[bank id acc-map]
(when-not (contains? bank id) ;;necessary check
  (assoc bank id acc-map)))
    
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
   (dosync 
     (doseq [[id acc] (partition 2 ids-accs)]
       (alter bank new-account id acc))) @bank)
     
(defn close-accounts!
"Entry point for closing accounts. 
 Expects a bank and any number of account ids to be deleted.
 Returns the modified bank." 
[bank & ids]
  (dosync 
     (alter bank delete-accounts ids)))
     
(defn total! 
 "Sum all accounts safely with a consistent view even if other transactions are still running."
 [bank]
   (dosync
     (reduce +'
       (map (comp :curr-balance second) @bank))))

(comment       

(dotimes [_ 300]
(future
 (transfer! bank (rand-int 41) 
                 (rand-int 50000) 
                 (rand-int 50000)))
(future (println (total! bank))) )

(every? #(= 100 %) (map :curr-balance @bank)) ; = false
(total! bank) ; = 5000000 otherwise something went wrong
                        
)      
                        
                
