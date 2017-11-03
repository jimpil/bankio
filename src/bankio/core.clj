(ns bankio.core
  (:import (java.util UUID)))


;;==============<DUMMY constructor/bank>============>
(defn- new-account
  [balance]
  {:uuid (UUID/randomUUID)
   :curr-balance balance
   :overdraft {:penalty 10
               :limit   100}})


(def DBANK ;; dummy bank
  (atom {"0000001" (ref (new-account 50))
         "0000002" (ref (new-account 50))
         "0000003" (ref (new-account 50))
         "0000004" (ref (new-account 50))
         "0000005" (ref (new-account 50))})
  )

;;============<PURE FUNCTIONS>=================
(defn credit
  "Adds <amount> to the current balance of <account>."
  [account amount]
  (update account :curr-balance +' amount)) ;;safe math at the lowest level

(defn- debit*
  "The actual debiting fn which adds an additional pernalty upon each overdraft.
 Penalty amount depends on the customer-account."
  [account curr-balance debit-amount]
  (let [res (-' curr-balance debit-amount)]
    (if (neg? res)
      (if  (> (get-in account [:overdraft :limit]) res)
        (throw (IllegalStateException. "Above overdraft limit!"))
        (-' res (get-in account [:overdraft :penalty])))
      res)))

(defn debit
  "Subtracts <amount> from the current balance of <account>."
  [account amount]
  (update account :curr-balance (partial debit* account) amount)) ;;safe math at the lowest level


;;==============<SIDE EFFECTING ON ACCOUNTS>===========
(defn deposit!
  "Modifies (via a stateful side-effect) the <account> in <bank>
   such that <amount> is credited to it."
  [bank account-no amount]
  (let [account (get @bank account-no)]
    (dosync
      (commute account credit amount))))

(defn withdraw!
  "Modifies (via a stateful side-effect) the <account> in <bank>
   such that <amount> is debited to it."
  [bank account-no amount]
  (let [account (get @bank account-no)]
    (dosync
      (alter account debit amount))))

(defn- transfer*
  "Private helper. To be used in a transaction."
  [bank amount from to]
  (let [snap @bank
        account-from (get snap from)
        account-to (get snap to)]
    (commute account-to credit amount)
    (alter account-from debit amount)))


(defn transfer!
  "Modifies (via a stateful side-effect) accounts <from> & <to>
   such that <amount> is debited from <from>, and debited to <to>."
  [bank & amount-from-to]
  (assert (every? #(= 3 (count %)) amount-from-to)
          "Need triplets of [amount from to]")
  (dosync
    (run! (partial apply transfer* bank) amount-from-to)))


;;==================<SNAPSHOTTING>=================
(defn snapshot-bank!
  "Snapshots the whole bank."
  [bank]
  (dosync
    (into {} (map #(update % 1 deref)) @bank)))

(defn snapshot-account!
  "Snapshots the account <account-no>."
  [bank account-no]
  (dosync
    @(get @bank account-no)))

(defn balance!
  "Returns the balance of this particular <account-no>."
  [bank account-no]
  (:curr-balance (snapshot-account! bank account-no)))


;; INTEREST
(defn- interest
  "A higher-order function responsible for calculating valid interest-rates (0-1 inclusive).
  Returns a fn which expects an amount to apply the rate to which in turn will return the amount to be added (see apply-interest)."
  [rate]
  (assert (and (>= 1 rate)
               (<= 0 rate))
          "Cannot accept an interest-rate less than 0 or greater than 1.")
  (fn [amount]
    (*' amount rate)))

(defn- apply-interest
  "Returns a new account with interest applied to its current balance.
 Typically, 'rate-fn' is constructed via 'interest' ."
  [account rate-fn]
  (credit account (rate-fn (:curr-balance account))))  ;or simply  (* rate (balance account))


(defn apply-interest!
  "Modifies bank such that the same interest-rate is applied to the balances of the accounts corresponding to ids.
Returns nil."
  [bank rate & account-nos]
  (let [rate-fn (interest rate)]
    (dosync
      (doseq [account (map (partial get bank) account-nos)]
        (alter account apply-interest rate-fn)))))



;;=============<SIDE EFFECTING ON THE BANK>=============

(defn open-accounts!
  [bank & accounts]
  (assert (every? #(= 2 (count %)) accounts)
          "Need tuples of [id account-map]")
  (assert (not-any? (partial contains? @bank) (map first accounts))
          "Cannot open an account which exists already!")
  (swap! bank into accounts))

(defn close-accounts!
  [bank & account-nos]
  (assert (every? (partial contains? @bank) account-nos)
          "Cannot close an account which doesn't exist!")
  (swap! bank apply dissoc account-nos))