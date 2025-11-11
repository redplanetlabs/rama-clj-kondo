(ns com.rpl.rama-hooks.demo-gallery-test
    "Tests for real-world Rama modules from the demo gallery.
  Verifies that complete, production-style modules transform correctly."
    (:require
     [clojure.test :refer [deftest is]]
     [com.rpl.rama-hooks :as rama]
     [com.rpl.test-helpers :refer [sexpr->node node->sexpr]]))

(def bank-transfer-module-code
     '(defmodule
       BankTransferModule
       [setup topologies]
       (declare-depot setup *transfer-depot (hash-by :from-user-id))
       (declare-depot setup *deposit-depot (hash-by :user-id))
       (let
        [mb (microbatch-topology topologies "banking")]
        (declare-pstate mb $$funds {Long Long})
        (declare-pstate
         mb
         $$outgoing-transfers
         {Long (map-schema
                String                 ; transfer-id
                (fixed-keys-schema
                 {:to-user-id Long
                  :amt        Long
                  :success?   Boolean})
                {:subindex? true})})
        (declare-pstate
         mb
         $$incoming-transfers
         {Long (map-schema
                String                 ; transfer-id
                (fixed-keys-schema
                 {:from-user-id Long
                  :amt          Long
                  :success?     Boolean})
                {:subindex? true})})
        (<<sources
         mb
         (source> *transfer-depot :> %microbatch)
         (%microbatch :> {:keys [*transfer-id *from-user-id *to-user-id *amt]})
         (local-select> [(keypath *from-user-id) (nil->val 0)] $$funds :> *funds)
         (>= *funds *amt :> *success?)
         (<<if
          *success?
          (<<ramafn
           %deduct
           [*curr]
           (:> (- *curr *amt)))
          (local-transform> [(keypath *from-user-id) (term %deduct)] $$funds))
         (local-transform>
          [(keypath *from-user-id *transfer-id)
           (termval
            {:to-user-id *to-user-id
             :amt        *amt
             :success?   *success?})]
          $$outgoing-transfers)
         (|hash *to-user-id)
         (<<if
          *success?
          (+compound $$funds {*to-user-id (aggs/+sum *amt)}))
         (local-transform>
          [(keypath *to-user-id *transfer-id)
           (termval
            {:from-user-id *from-user-id
             :amt          *amt
             :success?     *success?})]
          $$incoming-transfers)
         (source> *deposit-depot :> %microbatch)
         (%microbatch :> {:keys [*user-id *amt]})
         (+compound $$funds {*user-id (aggs/+sum *amt)})))))

(def bank-transfer-module-transformed-code
     '(defn
       BankTransferModule
       [setup topologies]
       (let
        [*transfer-depot (declare-depot setup (hash-by :from-user-id))]
        (pr *transfer-depot)
        (let
         [*deposit-depot (declare-depot setup (hash-by :user-id))]
         (pr *deposit-depot)
         (let
          [mb (microbatch-topology topologies "banking")]
          (let
           [$$funds (declare-pstate mb {Long Long})]
           (pr $$funds)
           (let
            [$$outgoing-transfers
             (declare-pstate
              mb
              {Long (map-schema
                     String ; transfer-id
                     (fixed-keys-schema
                      {:to-user-id Long
                       :amt        Long
                       :success?   Boolean})
                     {:subindex? true})})]
            (pr $$outgoing-transfers)
            (let
             [$$incoming-transfers
              (declare-pstate
               mb
               {Long (map-schema
                      String     ; transfer-id
                      (fixed-keys-schema
                       {:from-user-id Long
                        :amt          Long
                        :success?     Boolean})
                      {:subindex? true})})]
             (pr $$incoming-transfers)
             (fn
              []
              (let
               [%microbatch (source> *transfer-depot)]
               (let
                [{:keys [*transfer-id *from-user-id *to-user-id *amt]}
                 (%microbatch)]
                (let
                 [*funds
                  (local-select>
                   [(keypath *from-user-id) (nil->val 0)]
                   $$funds)]
                 (let
                  [*success? (>= *funds *amt)]
                  (when
                   *success?
                   (letfn
                    [(%deduct [*curr] (:> (- *curr *amt)))]
                    (local-transform>
                     [(keypath *from-user-id)
                      (term %deduct)]
                     $$funds)))

                  (local-transform>
                   [(keypath *from-user-id *transfer-id)
                    (termval
                     {:to-user-id *to-user-id
                      :amt        *amt
                      :success?   *success?})]
                   $$outgoing-transfers)
                  (|hash *to-user-id)
                  (when
                   *success?
                   (+compound
                    $$funds
                    {*to-user-id (aggs/+sum *amt)}))
                  (local-transform>
                   [(keypath *to-user-id *transfer-id)
                    (termval
                     {:from-user-id *from-user-id
                      :amt          *amt
                      :success?     *success?})]
                   $$incoming-transfers)))))
              (let
               [%microbatch (source> *deposit-depot)]
               (let
                [{:keys [*user-id *amt]} (%microbatch)]
                (+compound
                 $$funds
                 {*user-id (aggs/+sum *amt)}))))))))))))

(deftest demo-gallery-bank-transfer-module-test
  ;; Tests transformation of a bank transfer module with:
  ;; - Multiple depots (transfers and deposits)
  ;; - Multiple pstates (funds, outgoing/incoming transfers)
  ;; - Multiple sources in <<sources construct
  ;; - Conditional logic with <<if
  ;; - Nested <<ramafn for deducting funds
  ;; - Aggregations with +compound
  (is (= bank-transfer-module-transformed-code
         (node->sexpr
          (rama/defmodule-hook
           (sexpr->node bank-transfer-module-code))))))

(def profile-gallery-module-code
     '(defmodule
       ProfileModule
       [setup topologies]
       (declare-depot setup *registration-depot (hash-by :username))
       (declare-depot setup *profile-edits-depot (hash-by :user-id))
       (let
        [s (stream-topology topologies "profiles")
         id-gen (ModuleUniqueIdPState. "$$id")]
        (declare-pstate
         s
         $$username->registration
         {String ; username
          (fixed-keys-schema
           {:user-id Long
            :uuid    String})})
        (declare-pstate
         s
         $$profiles
         {Long ; user ID
          (fixed-keys-schema
           {:username      String
            :pwd-hash      String
            :display-name  String
            :height-inches Long})})
        (.declarePState id-gen s)
        (<<sources
         s
         (source> *registration-depot :> {:keys [*uuid *username *pwd-hash]})
         (local-select>
          (keypath *username)
          $$username->registration
          :>
          {*curr-uuid :uuid :as *curr-info})
         (<<if
          (or>
           (nil? *curr-info)
           (= *curr-uuid *uuid))
          (java-macro! (.genId id-gen "*user-id"))
          (local-transform>
           [(keypath *username)
            (multi-path
             [:user-id (termval *user-id)]
             [:uuid (termval *uuid)])]
           $$username->registration)
          (|hash *user-id)
          (local-transform>
           [(keypath *user-id)
            (multi-path
             [:username (termval *username)]
             [:pwd-hash (termval *pwd-hash)])]
           $$profiles))
         (source> *profile-edits-depot :> {:keys [*user-id *edits]})
         (explode *edits :> {:keys [*field *value]})
         (local-transform>
          [(keypath *user-id *field) (termval *value)]
          $$profiles)))))

(def profile-gallery-module-transformed-code
     '(defn
       ProfileModule
       [setup topologies]
       (let
        [*registration-depot (declare-depot setup (hash-by :username))]
        (pr *registration-depot)
        (let
         [*profile-edits-depot (declare-depot setup (hash-by :user-id))]
         (pr *profile-edits-depot)
         (let
          [s (stream-topology topologies "profiles")
           id-gen (ModuleUniqueIdPState. "$$id")]
          (let
           [$$username->registration
            (declare-pstate
             s
             {String
              (fixed-keys-schema
               {:user-id Long
                :uuid    String})})]
           (pr $$username->registration)
           (let
            [$$profiles
             (declare-pstate
              s
              {Long (fixed-keys-schema
                     {:username      String
                      :pwd-hash      String
                      :display-name  String
                      :height-inches Long})})]
            (pr $$profiles)
            (.declarePState id-gen s)
            (fn
             []
             (let
              [{:keys [*uuid *username *pwd-hash]}
               (source> *registration-depot)]
              (let
               [{:as *curr-info *curr-uuid :uuid}
                (local-select>
                 (keypath *username)
                 $$username->registration)]
               (when
                (or>
                 (nil? *curr-info)
                 (= *curr-uuid *uuid))
                (let
                 [_ (pr) [*user-id] []]
                 (pr [*user-id])
                 (.genId id-gen "*user-id")
                 (local-transform>
                  [(keypath *username)
                   (multi-path
                    [:user-id (termval *user-id)]
                    [:uuid (termval *uuid)])]
                  $$username->registration)
                 (|hash *user-id)
                 (local-transform>
                  [(keypath *user-id)
                   (multi-path
                    [:username (termval *username)]
                    [:pwd-hash (termval *pwd-hash)])]
                  $$profiles)))))
             (let
              [{:keys [*user-id *edits]}
               (source> *profile-edits-depot)]
              (let
               [{:keys [*field *value]} (explode *edits)]
               (local-transform>
                [(keypath *user-id *field) (termval *value)]
                $$profiles)))))))))))

(deftest demo-gallery-profile-module-test
  ;; Tests transformation of a user profile module with:
  ;; - Multiple depots (registration, profile edits)
  ;; - Multiple pstates (username->registration, profiles)
  ;; - Java interop with ModuleUniqueIdPState
  ;; - java-macro! for ID generation
  ;; - Multi-path transforms
  ;; - Explode for processing collections
  (is (= profile-gallery-module-transformed-code
         (node->sexpr
          (rama/defmodule-hook
           (sexpr->node profile-gallery-module-code))))))

(def time-series-module-code
     '(defmodule
       TimeSeriesModule
       [setup topologies]
       (declare-depot setup *render-latency-depot (hash-by :url))
       (let
        [mb (microbatch-topology topologies "timeseries")]
        (declare-pstate
         mb
         $$window-stats
         {String                ; url
          {clojure.lang.Keyword ; granularity
           (map-schema
            Long                ; bucket
            WindowStats
            {:subindex? true})}})
        (<<sources
         mb
         (source> *render-latency-depot :> %microbatch)
         (%microbatch :> {:keys [*url *render-millis *timestamp-millis]})
         (single-window-stat *render-millis :> *single-stat)
         (emit-index-granularities *timestamp-millis :> *granularity *bucket)
         (+compound
          $$window-stats
          {*url
           {*granularity
            {*bucket (+combine-measurements *single-stat)}}}))
        (<<query-topology
         topologies
         "get-stats-for-minute-range"
         [*url *start-bucket *end-bucket :> *stats]
         (|hash *url)
         (explode
          (query-granularities :m *start-bucket *end-bucket)
          :> [*granularity *gstart *gend])
         (local-select>
          [(keypath *url *granularity)
           (sorted-map-range *gstart *gend)
           MAP-VALS]
          $$window-stats
          :> *bucket-stat)
         (|origin)
         (+combine-measurements *bucket-stat :> *stats)))))

(def time-series-module-transformed-code
     '(defn
       TimeSeriesModule
       [setup topologies]
       (let
        [*render-latency-depot (declare-depot setup (hash-by :url))]
        (pr *render-latency-depot)
        (let
         [mb (microbatch-topology topologies "timeseries")]
         (let
          [$$window-stats
           (declare-pstate
            mb
            {String              ; url
             {clojure.lang.Keyword ; granularity
              (map-schema
               Long              ; bucket
               WindowStats
               {:subindex? true})}})]
          (pr $$window-stats)
          (fn
           []
           (let
            [%microbatch (source> *render-latency-depot)]
            (let
             [{:keys [*url *render-millis *timestamp-millis]} (%microbatch)]
             (let
              [*single-stat (single-window-stat *render-millis)]
              (let
               [[*granularity *bucket]
                (emit-index-granularities *timestamp-millis)]
               (+compound
                $$window-stats
                {*url
                 {*granularity
                  {*bucket (+combine-measurements
                            *single-stat)}}}))))))
          (fn
           get-stats-for-minute-range
           [*url *start-bucket *end-bucket]
           (|hash *url)
           (let
            [[*granularity *gstart *gend]
             (explode
              (query-granularities
               :m
               *start-bucket
               *end-bucket))]
            (let
             [*bucket-stat
              (local-select>
               [(keypath *url *granularity)
                (sorted-map-range *gstart *gend)
                MAP-VALS]
               $$window-stats)]
             (|origin)
             (let
              [*stats (+combine-measurements *bucket-stat)]
              [*stats])))))))))

(deftest demo-gallery-time-seties-module-test
  ;; Tests transformation of a time series module with:
  ;; - Window stats pstate with nested map schemas
  ;; - <<sources with microbatch processing
  ;; - Multiple emit captures (granularity and bucket)
  ;; - +compound with nested aggregations
  ;; - <<query-topology for query operations
  ;; - Explode and local-select> with sorted-map-range
  (is (= time-series-module-transformed-code
         (node->sexpr
          (rama/defmodule-hook
           (sexpr->node time-series-module-code))))))

(def top-users-module-code
     '(defmodule
       TopUsersModule
       [setup topologies]
       (declare-depot setup *purchase-depot (hash-by :user-id))
       (let
        [mb (microbatch-topology topologies "topusers")]
        (declare-pstate mb $$user-total-spend {Long Long})
        (declare-pstate mb $$top-spending-users java.util.List {:global? true})
        (<<sources
         mb
         (source> *purchase-depot :> %microbatch)
         (<<batch
          (<<if
           true
           (identity 1 :> *x)
           (else>)
           (identity 2 :> *x))
          (pr *x)
          (loop<-
           [*x [1 2 3] :> *ret])
          (user-spend-subbatch %microbatch :> *user-id *total-spend-cents)
          (vector *user-id *total-spend-cents :> *tuple)
          (|global)
          (+top-monotonic
           [TOP-AMOUNT]
           $$top-spending-users
           *tuple
           :+options
           {:id-fn       first
            :sort-val-fn last}))))))

(def top-users-module-transformed-code
     '(defn
       TopUsersModule
       [setup topologies]
       (let
        [*purchase-depot (declare-depot setup (hash-by :user-id))]
        (pr *purchase-depot)
        (let
         [mb (microbatch-topology topologies "topusers")]
         (let
          [$$user-total-spend (declare-pstate mb {Long Long})]
          (pr $$user-total-spend)
          (let
           [$$top-spending-users
            (declare-pstate mb java.util.List {:global? true})]
           (pr $$top-spending-users)
           (fn
            []
            (let
             [%microbatch (source> *purchase-depot)]
             (<<batch
              (let
               [*x nil
                _
                (if
                 true
                 (let [*x (identity 1)] {*x *x})
                 (let [*x (identity 2)] {*x *x}))]
               (pr *x)
               (let
                [[*ret] (let [*x [1 2 3]])]
                (let
                 [[*user-id *total-spend-cents]
                  (user-spend-subbatch %microbatch)]
                 (let
                  [*tuple (vector *user-id *total-spend-cents)]
                  (|global)
                  (+top-monotonic
                   [TOP-AMOUNT]
                   $$top-spending-users
                   *tuple
                   :+options
                   {:id-fn       first
                    :sort-val-fn last}))))))))))))))

(deftest demo-gallery-top-users-module-test
  ;; Tests transformation of a top users module with:
  ;; - Global pstate for top spending users
  ;; - <<batch for batch processing
  ;; - Nested <<if inside batch
  ;; - loop<- for iteration
  ;; - +top-monotonic aggregation with options
  ;; - |global partitioning
  (is (= top-users-module-transformed-code
         (node->sexpr
          (rama/defmodule-hook
           (sexpr->node top-users-module-code))))))
