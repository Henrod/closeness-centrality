(ns closeness-centrality.server-test
  (:require [clojure.test :refer [is deftest testing]]
                     [closeness-centrality.server :refer :all]
                     [ring.mock.request :as mock]))

(deftest test-wrong-path
	(reset! graph {})
    (reset! frauds #{})
    (is (= (app (mock/request :get "/invalid"))
                {:headers {"Content-Type" "text/html; charset=utf-8"}
                  :body "Not Found"
                  :status 404})))

(deftest test-add-connection
	(testing "add first connection"
      (is (= (app (mock/request :post "/add" {"src" 1, "dst" 2}))
                  {:headers {}
                    :body "Done"
                    :status 200}))
	  (is (= (app (mock/request :get "/"))
	  	          {:headers {}
	  	            :status 200
	  	            :body "([\"1\" 1.0] [\"2\" 1.0])"})))

	(testing "add second connection"
      (is (= (app (mock/request :post "/add" {"src" 1, "dst" 3}))
                  {:headers {}
                    :body "Done"
                    :status 200}))
	  (is (= (app (mock/request :get "/"))
	  	          {:headers {}
	  	            :status 200
	  	            :body "([\"1\" 0.5] [\"2\" 0.3333333333333333] [\"3\" 0.3333333333333333])"})))

	(testing "add third connection"
      (is (= (app (mock/request :post "/add" {"src" 2, "dst" 4}))
                  {:headers {}
                    :body "Done"
                    :status 200}))
	  (is (= (app (mock/request :get "/"))
	  	          {:headers {}
	  	            :status 200
	  	            :body "([\"1\" 0.25] [\"2\" 0.25] [\"3\" 0.1666666666666667] [\"4\" 0.1666666666666667])"}))))

(deftest test-add-fraudulent
	(testing "1 is fraudulent"
		(is (= (app (mock/request :post "/fraud" {"fraud" 1}))
			        {:headers {}
			          :status 200
			          :body "Done"}))
		(is (= (app (mock/request :get "/"))
			        {:headers {}
			          :status 200
			          :body "([\"2\" 0.125] [\"4\" 0.125] [\"3\" 0.08333333333333333] [\"1\" 0.0])"})))

	(testing "3 is also fraudulent"
		(is (= (app (mock/request :post "/fraud" {"fraud" 3}))
			        {:headers {}
			          :status 200
			          :body "Done"}))
		(is (= (app (mock/request :get "/"))
			        {:headers {}
			          :status 200
			          :body "([\"4\" 0.109375] [\"2\" 0.09375] [\"1\" 0.0] [\"3\" 0.0])"})))

	(testing "Adding 3 again does not change anything"
		(is (= (app (mock/request :post "/fraud" {"fraud" 3}))
			        {:headers {}
			          :status 200
			          :body "Done"}))
		(is (= (app (mock/request :get "/"))
			        {:headers {}
			          :status 200
			          :body "([\"4\" 0.109375] [\"2\" 0.09375] [\"1\" 0.0] [\"3\" 0.0])"})))

  (testing "add same source and destination"
    (is (= (app (mock/request :post "/add" {"src" 5, "dst" 5}))
           {:headers {}
            :body "Done"
            :status 200})))

  (testing "getting new rank"
    (is (= (app (mock/request :get "/"))
           {:headers {}
            :status 200
            :body "([\"4\" 0.109375] [\"2\" 0.09375] [\"1\" 0.0] [\"3\" 0.0])"}))))
