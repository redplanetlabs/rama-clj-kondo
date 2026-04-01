(ns com.rpl.rama-hooks.external-projects-test
    (:require
     [clj-kondo.core :as clj-kondo]
     [clojure.edn :as edn]
     [clojure.java.io :as io]
     [clojure.java.shell :as shell]
     [clojure.pprint :as pp]
     [clojure.string :as str]
     [clojure.test :refer [deftest is testing]]))

(def ^:private projects
     (edn/read-string
      (slurp "test-regression/com/rpl/rama_hooks/projects.edn")))

(defn- sh!
       [dir & args]
       (let [{:keys [exit out err]} (apply shell/sh (concat args [:dir dir]))]
            (when-not (zero? exit)
                      (throw
                       (ex-info (str "Command failed: " (str/join " " args))
                                {:dir dir
                                 :args args
                                 :exit exit
                                 :out out
                                 :err err})))
            out))

(defn- delete-tree!
       [f]
       (when (.exists f)
             (when (.isDirectory f)
                   (doseq [child (.listFiles f)]
                          (delete-tree! child)))
             (.delete f)))

(defn- relative-path
       [root file]
       (let [root-path (.toPath (.getCanonicalFile (io/file root)))
             file-path (.toPath (.getCanonicalFile (io/file file)))]
            (-> (str (.relativize root-path file-path))
                (str/replace "\\" "/"))))

(defn- ensure-checkout!
       [{:keys [name repo sha]}]
       (let [checkouts-dir (io/file "test-regression" "checkouts")
             project-dir (io/file checkouts-dir name)]
            (.mkdirs checkouts-dir)
            (when-not (.exists project-dir)
                      (sh! (.getPath checkouts-dir)
                           "git" "clone" "--no-checkout" "--depth" "1" repo name))
            (sh! (.getPath project-dir) "git" "fetch" "--depth" "1" "origin" sha)
            (sh! (.getPath project-dir) "git" "checkout" "--force" sha)
            (.getPath project-dir)))

(defn- update-fixture?
       []
       (some? (System/getenv "CLJ_KONDO_REGRESSION_UPDATE")))

(defn- lint-project
       [{:keys [paths local] :as project}]
       (let [project-dir (if local "." (ensure-checkout! project))
             config-dir (if local
                            (io/file "." ".clj-kondo")
                            (io/file project-dir ".clj-kondo"))
             _ (when-not local
                         (delete-tree! (io/file config-dir ".cache")))
             _ (clj-kondo/run! {:config-dir (.getPath config-dir)
                                :copy-configs true
                                :skip-lint true
                                :lint [(System/getProperty "java.class.path")]})
             lint-paths (if local
                            (mapv str paths)
                            (mapv #(str (io/file project-dir %)) paths))
             findings (:findings (clj-kondo/run! {:config-dir (.getPath config-dir)
                                                  :lint lint-paths
                                                  :parallel true
                                                  :repro true}))]
            (->> findings
                 (map (fn [finding]
                          (-> (select-keys finding [:filename :row :col :level :type :message])
                              (update :filename #(relative-path project-dir %)))))
                 (sort-by (juxt :filename :row :col :type :message))
                 vec)))

(defn- assert-findings!
       [{:keys [name findings-file] :as project}]
       (let [actual-findings (lint-project project)
             findings-file (io/file findings-file)
             _ (when (update-fixture?)
                     (.mkdirs (.getParentFile findings-file))
                     (spit findings-file (with-out-str (pp/pprint actual-findings))))
             expected-findings (edn/read-string (slurp findings-file))]
            (is (= expected-findings actual-findings)
                (str "Findings changed for " name
                     ". Set CLJ_KONDO_REGRESSION_UPDATE=1 to update fixture."))))

(deftest linting-regression-test
  (doseq [project projects]
         (testing (:name project)
                  (assert-findings! project))))
