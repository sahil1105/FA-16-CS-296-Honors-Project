(ns adventure.core
  (:require [clojure.core.match :refer [match]]
            [clojure.string :as str])
  (:gen-class))

(def the-map
  {
    :staircase {:desc "You are near the staircase on the 1st floor. There is a note on the couch. Might it be for you?"
                :title "near-the-staircase"
                :dir {:south :ACM-Office
                      :north :Einstein
                      :west :Garden
                      :east :WCS-Office
                      :upstairs :Chilling-Area
                      :downstairs :Study-Area
                      }
                :contents #{"note"}
                }

    :ACM-Office {:desc "You are in the ACM Office, the place where all the cool kids hang out. There is your laptop you've been looking for. Wait, there's also a phone, with an urgent notification."
                 :title "inside the acm-office"
                 :dir {:north :staircase
                       :south :outside
                       :west :academic_office
                       }
                 :contents #{"laptop", "phone"}
                 }
    :Einstein {:desc "There is no one here. Strange. Some bagels though. I guess you can grab one."
               :title "the einstein bagel shop"
               :dir {:south :staircase
                     :north :lecture_hall
                     }
               :contents #{"bagel", "energy_drink", "coffee"}
               }
    :lecture_hall {:desc "The big classroom you might have been in during 125. There is the first question on the giant screen. Read it."
                   :title "the 1404 lecture hall"
                   :dir {:south :Einstein
                         }
                   :contents #{"first_question", "computer"}
                   }
    :Garden {:desc "No one here either. Lovely place though, if only it had wifi."
             :tile "the garden across siebel which no one uses"
             :dir {:west :Study-Area
                   :east :staircase
                   }
             :contents #{}
             }
    :WCS-Office {:desc "small office, not much around"
                 :title "the WCS Office"
                 :dir {:west :staircase
                 }
                 :contents #{"flash_light"}
                 }
    :academic_office {:desc "Where every CS Major is on drop deadline"
                      :tile "The Academic Office"
                      :dir {:west :ACM-Office
                            }
                      :contents #{"all_access_pass"}
                      }
    :outside {:desc "Not much here, what else did you expect? Lets go back to the ACM Office"
              :tile "Outside Siebel"
              :dir {:north :ACM-Office
                    }
              :contents #{}
              }
    :Study-Area {:desc "It's tuesday evening and no one's here? Something must really be up."
                 :title "the study area, without any plugs to charge our laptops"
                 :dir {:upstairs :staircase
                       :west :Computer-Lab
                       :east :Garden
                       }
                 :contents #{}
                 }
    :Computer-Lab {:desc "Where all the 241 kids are always holed up"
                   :title "One of the Computer Labs, take your pick"
                   :dir {:east :Study-Area
                         }
                   :contents #{}
                   }
    :Drop-Boxes {:desc "You may remember dropping your 173 Honors HWs here. Look one of them is open."
                 :title "The drop boxes"
                 :dir {:north :Study-Area
                       }
                 :contents #{"second_question"}
                 }
    :Classroom {:desc "The other big lecture hall."
                :title "A lecture Hall"
                :dir {:downstairs :staircase
                      :north :VR_Lab
                      :south :Chilling-Area
                      :west :Destination
                      }
                :contents #{}
                }
    :VR_Lab {:desc "Where all the kids want to work at some point. One of the VR Headsets is powered on. Lets check it out. Why don't you 'try the VR Headset'?"
             :title "The VR Lab"
             :dir {:south :Classroom
                   }
             :contents {"VR_Headset", "final_question"}
             }
    :Chilling-Area {:desc "You come here when you get into your Tec Electives I guess."
                    :title "The chilling area on the 2nd floor"
                    :dir {:north :Classroom
                          :east :Repair-Lab
                          }
                    :contents #{}
                    }
    :Repair-Lab {:desc "Never been here. They say they can fix anything here. Might be worth it to pick up a few tools. I see..."
                 :title "The repair lab or whatever it's called"
                 :dir {:west :Chilling-Area
                       }
                 :contents #{"toolkit", "wrench", "goggles"}
                 }
    :Destination {:desc "The final stop. Type 'enter' to enter the room. Once you are in the room type 'interact' to interact with the computer to answer the three questions you found to release your friends."
                  :title "The final destination"
                  :dir {:east :Classroom
                        }
                  :contents #{"the_computer"}
                  :unlocked false
                  }
    }
  )

  (def adventurer
    {:name ""
     :location :staircase
     :inventory #{}
     :hasQuestion1 false
     :hasQuestion2 false
     :hasQuestion3 false
     :answeredAllThree false
     :seen #{}
     }
    )

(def introMessage "Hi, You are near the staircase on the 1st floor of Siebel Center. There's no one around. You can move around and find what's going on. Type 'help' to get a list of commands. Type 'status' to get more info about where you are. Print 'inventory' to get a list of your inventory at any time.")

(defn status[player]
  (let [location (player :location)]
    (println (str "You are " (-> the-map location :title) ". "))
    (when-not ((player :seen) location)
      (println (-> the-map location :desc)))
    (update-in player [:seen] #(conj % location))
    (println (str "Has question one? " (player :hasQuestion1)))
    (println (str "Has question two? " (player :hasQuestion2)))
    (println (str "Has question three? " (player :hasQuestion2)))))

(defn go [dir player]
  (let [location (player :location)
        dest (->> the-map location :dir dir)]
    (if (nil? dest)
      (do (println "You can't go that way.")
          player)
      (assoc-in player [:location] dest))))

(defn print_inventory [player]
    (do (println (seq (player :inventory)))
      player))

(def help "Here are some of the commands you can use: \n Directions: \n north, south, west, east, upstairs, downstairs \n Actions: \n pickup, read, get, eat, drink, interact, chill, break, release_your_friends, enter, answer")

(defn puzzle [player]
  (println "Enter your answers to the three questions, one per line.")
  (let [answer1 (read-line)
        answer2 (read-line)
        answer3 (read-line)]
    (if (and (= answer1 "mary") (= answer2 "secret") (= answer3 "name") ) (do (println "Your answers are correct. Enter 'release' to release your friends") (assoc-in player [:answeredAllThree] true) player)
      (do (println "One or more of your answers are incorrect. Enter 'interact' again to retry.") player))
    )
  )





(defn interact [player]
  (let [location (player :location)
        q1 (player :hasQuestion1)
        q2 (player :hasQuestion2)
        q3 (player :hasQuestion3)
        inv (player :inventory)
        unlocked (->> the-map :Destination :unlocked)
        ]
    (cond (= location :lecture_hall) (do (println "Your friends need to be rescued. What are you still doing here?") player)
          (= location :VR_Lab) (do (println "Here's your second question: ..... To store it in the inventory, type 'get question_two'")
                                        (assoc-in player [:hasQuestion2] true) player)
          (and (= location :Destination) (q1) (q2) (q3) (contains? inv "all access pass") (unlocked)) (puzzle player)
          :else (do (println "Invalid command at this point") player)
    )))

(defn release [player]
  (let [ans (player :answeredAllThree)]
    (if (ans) (do (println "Congratulations! You have rescued your friends.") player) (do (println "Sorry, you haven't answered all the questions yet") player) )
    ))

(defn enter [player]
  (let [location (player :location)
        inv (player :inventory)]
    (if (and (= location :Destination) (contains? inv "all access pass")) (do (println "Enter 'use all_access_pass' to enter the room.") player) (do (println "You are either not at the destination, or you don't have the all access pass.") player)
      )))

(defn answer [player]
  (if (= (player :location) (:Destination)) (interact player) (do (println "Wrong place to call the command in") player)))

(defn pickup [object player]
  (let [location (player :location)
        inv (player :inventory)
        unlocked (->> the-map :Destination :unlocked)
        ]
    (cond (and (= location :staircase) (= object "note")) ((update-in player [:inventory] #(disj % "note")) player)
          (and (= location :WCS-Office) (= object "flash_light")) ((update-in player [:inventory] #(disj % "flash_light")) player)
          (and (= location :academic_office) (= object "all_access_pass")) ((update-in player [:inventory] #(disj % "all_access_pass")) player)
          (and (= location :Repair-Lab) (= object "toolkit")) ((update-in player [:inventory] #(disj % "toolkit")) player)
          (and (= location :Repair-Lab) (= object "wrench")) ((update-in player [:inventory] #(disj % "wrench")) player)
          (and (= location :Repair-Lab) (= object "goggles")) ((update-in player [:inventory] #(disj % "goggles")) player)
          :else (do (println "Can't pick that up") player)
          )))

(defn read [object player]
  (let [location (player :location)
        inv (player :inventory)]
    (cond (and (contains? inv "note") (= object "note")) (do (println note) player)
          (and (contains? inv "phone") (= object "notification")) (do (println notification) player)
          (and (contains? inv "first_question") (= object "first_question")) (do (println "Mary’s father has 5 daughters – Nana, Nene, Nini, Nono. What is the fifth daughters name? ") player)
          (and (contains? inv "second_question") (= object "second_question")) (do (println "If I have it, I don’t share it. If I share it, I don’t have it. What is it? ") player)
          (and (contains? inv "third_question") (= object "third_question")) (do (println "What belongs to you, but is used by others more than you?") player)
          :else (do (println "Can't read that") player)
          )))

(def note "")
(def notification "")


(defn eat [object player]
  (let [inv (player :inventory)]
    (if (and (contains? inv "bagel") (= object "bagel")) (do (println "Okay. That should give you enough energy for the task") player) (do (println "Can't eat that") player)
      )))

(defn drink [object player]
  (let [inv (player :inventory)]
    (cond (and (contains? inv "coffee") (= object "coffee")) (do (println "Done.") player)
          (and (contains? inv "energy_drink") (= object "energy_drink")) (do (println "Done.") player)
          :else (do (println "Can't drink that") player)
          )))

(defn use [object player]
  (let [inv (player :inventory)
        location (player :location)]
    (cond (and (contains? inv "flash_light") (= object "flash_light")) (do (println "I can do that but it's middle of the day and you'd just be wasting batteries.") player)
          (and (= location :Destination) (= object "all_access_pass")) (do (println "Alright, the door is unlocked") (assoc-in (-> the-map :Destination :unlocked) true) player)
          :else (do (println "Can't use that") player))))

(defn get [object player]
  (let [inv (player :inventory)
        location (player :location)]
    (cond (and (= location :ACM-Office) (= object "laptop")) (do (println "Laptop added to inventory") ((update-in player [:inventory] #(disj % "laptop")) player))
          (and (= location :ACM-Office) (= object "phone")) (do (println "Phone added to inventory") ((update-in player [:inventory] #(disj % "phone")) player))
          (and (= location :Einstein) (= object "bagel") (do (println "Bagel added to inventory") ((update-in player [:inventory] #(disj % "bagel")) player)))
          (and (= location :Einstein) (= object "energy_drink") (do (println "Energy Drink added to inventory") ((update-in player [:inventory] #(disj % "energy_drink")) player)))
          (and (= location :Einstein) (= object "coffee") (do (println "Coffee added to inventory") ((update-in player [:inventory] #(disj % "coffee")) player)))
          (and (= location :lecture_hall) (= object "first_question") (do (println "First Question added to inventory") ((update-in player [:inventory] #(disj % "first_question")) player)))
          (and (= location :Drop-Boxes) (= object "second_question") (do (println "Second Question added to inventory") ((update-in player [:inventory] #(disj % "second_question")) player)))
          (and (= location :VR_Lab) (= object "third_question") (do (println "Third Question added to inventory") ((update-in player [:inventory] #(disj % "third_question")) player)))
          :else (do (println "Can't get that") player)
          )))



(defn respond [player inst]
  (if (contains? inst 1)
  (match [(inst 0)]
         [:pickup] (pickup (inst 1) player)
         [:read] (read (inst 1) player)
         [:get] (get (inst 1) player)
         [:eat] (eat (inst 1) player)
         [:drink] (drink (inst 1) player)
         [:use] (use (inst 1) player)
         )


    (match [(inst)]
         [:north] (go :north player)
         [:south] (go :south player)
         [:east] (go :east player)
         [:west] (go :west player)
         [:upstairs] (go :upstairs player)
         [:downstairs] (go :downstairs player)
         [:inventory] (print_inventory player)
         [:help] (do (println help) player)
         [:status] (status player)
         [:interact] (interact player)
         [:chill] (do (println "You're now refreshed") player)
         [:break] (do (println "This is Siebel. We don't break things here") player)
         [:release_your_friends] (release player)
         [:enter] (enter player)
         [:answer] (answer player)
           (do
             (println (str "Invalid input "(-> adv :name)". Recheck and enter again."))
             player)
           )
    ))

(defn to-keywords [commands]
  (mapv keyword (str/split commands #"[.,?! ]+")))

(defn -main
  [& args]
  (println "What is your name?")
  (let [n (read-line)
        adv-n (adventurer :name)
        adv' (assoc-in adventurer [:name] n)]
    (println introMessage)
  (loop [local-map the-map
         local-player adventurer]
    (let [pl (status local-player)
          _  (println "What do you want to do?")
          command (read-line)]
      (recur local-map (respond pl (to-keywords command)))))))

(defn -main
  [& args]
  (println "What is your name?")
  (let [n (read-line)
        adv-n (adventurer :name)
        adv' (assoc-in adventurer [:name] n)]
    (println introMessage)
  (loop [local-map the-map
         local-player adventurer]
    (let [pl (status local-player)
          _  (println "What do you want to do?")
          command (read-line)]
      (recur the-map (respond pl (to-keywords command)))))))



