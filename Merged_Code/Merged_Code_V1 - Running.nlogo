extensions [csv]   ;; Load CSV extension

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Declare Breeds & Variables ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
globals [radius threshold adjacency-matrix firm-ids]

breed [firms firm]
breed [customers customer]

firms-own [
  firm_ID
  latitude
  longitude
  capacity
  overbook
  customers_count
  bookings
  customers_waiting
  pre_decision_booking
  franchise_ID
  star_rating
  firm_age
  f_deviance_propensity
  firm_decision
  firm_status
  delta_bookings
  perceived-performance       ;; Calculated network performance influence
]

customers-own [
  customer_ID
  latitude
  longitude
  stickiness
  random_number
  current_firm
  previous_firm
  decision
  c_deviance_propensity
  utility
]

;;;;;;;;;;;;;;;;;;;;;;;
;;; Setup Procedure ;;;
;;;;;;;;;;;;;;;;;;;;;;;

to setup
  clear-all
  set radius 15
  set threshold 2.5 ;;minimum utility a customer willing to accept to choose a firm
  import-firm-initialization "Firm_initialization_v20.csv"
  import-customers-initialization "Customer_initialization_v20.csv"
  set adjacency-matrix load-adjacency-matrix "newnetwork.csv"
  define-network
  ;; link each customer to the firm with matching ID.
  ask firms [
    ;; Inside this block, 'self' is a firm, which has 'firm_ID' but not 'current_firm'

    ask customers with [ current_firm = [firm_ID] of myself ] [
      ;; Now, inside this block, 'self' is a customer
      create-link-with myself  ;; Link this customer to the firm that asked me
    ]
  ]


  reset-ticks
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Firm to firm connection ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to-report load-adjacency-matrix [filename]
  let raw-matrix csv:from-file filename
  let data but-first raw-matrix
  let selected-matrix map [row -> butfirst row] data
  print (word "Loaded Adjacency Matrix: " selected-matrix)
  report selected-matrix
end

to define-network
  if not is-list? adjacency-matrix [
    print "ERROR: adjacency-matrix is not loaded. Run load-adjacency-matrix first."
    stop
  ]

  print (word "Firms: " firm-ids)

  if empty? firm-ids [
    print "ERROR: No firms found. Run setup first."
    stop
  ]

  let num-firms length firm-ids

  if num-firms != length adjacency-matrix [
    print (word "ERROR: Adjacency matrix size (" length adjacency-matrix ") does not match firm count (" num-firms ").")
    stop
  ]
 ;; Loop to create links based on adjacency matrix
  foreach range num-firms [ i ->
    foreach range num-firms [ j ->
      if i != j and item j (item i adjacency-matrix) = 1 [
        let firm1 one-of firms with [firm_ID = (i + 1)]
        let firm2 one-of firms with [firm_ID = (j + 1)]

        if firm1 != nobody and firm2 != nobody [
          ask firm1 [
            if not link-neighbor? firm2 [
              create-link-with firm2 [
                set color red
              ]
              print (word "Connected: " firm_ID " → " [firm_ID] of firm2)
            ]
          ]
        ]
      ]
    ]
  ]

  ;layout-spring turtles links 0.5 10 2
  print "✅ Network successfully established!"
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Tick 1--Firm 1 Decision Procedure ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to tick-1-focal-decision
  let f one-of firms with [firm_ID = 1]
  if f != nobody [
    ask f [
      let threshold1 0
      let threshold2 0
      let c1 0.5  ;; for increasing capacity
      let c2 0.5  ;; for decreasing capacity
      let net-capacity (capacity - bookings)
      let max_capacity 75;  max capacity should be 75
     ;;rease capacity if low available capacity and high waiting demand ===
      if (net-capacity <= threshold1) and (customers_waiting >= threshold2) [
        let increase-amount ceiling (customers_waiting * c1)
        set capacity capacity + increase-amount
        if capacity > max_capacity[
          set capacity max_capacity
        ]
        print (word "✅ " firm_ID " ↑ increased capacity by " increase-amount " → New capacity: " capacity)
        set pre_decision_booking bookings
      ]

      ;; === Decrease capacity if excess free capacity and low waiting demand ===
      if (net-capacity >= threshold1) and (customers_waiting <= threshold2) [
        let decrease-amount ceiling ((capacity - bookings) * c2)
        set capacity max (list (capacity - decrease-amount) 0)
        print (word "⚠️ " firm_ID " ↓ reduced capacity by " decrease-amount " → New capacity: " capacity)
        set pre_decision_booking bookings
      ]
      ;; Final snapshot
      print (word "📌 " firm_ID " cap=" capacity ", bookings=" bookings ", waiting=" customers_waiting)
    ]
  ]
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;   TICK 5      ;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to tick-5-react-to-firm1
  ;; === Setup thresholds and parameters ===
  let firm1 one-of firms with [firm_ID = 1]
  let delta1 [bookings] of firm1 - [pre_decision_booking] of firm1  ;; ∆bookings of focal firm
  let threshold1 0  ;; Used to evaluate net capacity
  let threshold2 0  ;; Used to evaluate customers waiting
  let threshold3 3  ;; Threshold on ∆bookings after decay
  let c1 0.5         ;; % of waiting customers used to increase capacity
  let c2 0.5         ;; % of excess capacity used to decrease capacity

  ;; Print focal firm ∆bookings
  print (word "∆bookings of firm_1: " ceiling delta1)

  ;; === Step 1: React for Directly Connected Firms (1-hop neighbors) ===
  ask firms with [firm_ID != 1 and link-neighbor? firm1] [
    let raw-delta ceiling (delta1 * 0.8)                    ;; Apply 20% decay
    let inferred-delta raw-delta          ;; Round for threshold comparison
    let net-capacity (capacity - bookings)

    print (word firm_ID " (Direct) sees ∆=" inferred-delta
          " | net-cap=" ceiling net-capacity
          " | waiting=" ceiling customers_waiting)

    ;; === Decision to Increase Capacity ===
    if (inferred-delta >= threshold3) and
       (net-capacity <= threshold1) and
       (customers_waiting >= threshold2) [
      let change ceiling (customers_waiting * c1)
      set capacity capacity + change
      print (word "✅ " firm_ID " ↑ Direct ↑ capacity=" ceiling capacity
            ", bookings=" ceiling bookings
            ", client_count=" ceiling (bookings + customers_waiting))
      set pre_decision_booking bookings
    ]

    ;; === Decision to Decrease Capacity ===
    if (inferred-delta <= (-1 * threshold3)) and
       (net-capacity >= threshold1) and
       (customers_waiting <= threshold2) [
      let change ceiling ((capacity - bookings) * c2)
      set capacity max (list (capacity - change) 0)
      print (word "⚠️ " firm_ID " ↓ Direct ↓ capacity=" ceiling capacity
            ", bookings=" ceiling bookings
            ", client_count=" ceiling (bookings + customers_waiting))
      set pre_decision_booking bookings
    ]

    ;; === No Change if none of the conditions met ===
    if not (
      (inferred-delta >= threshold3 and net-capacity <= threshold1 and customers_waiting >= threshold2) or
      (inferred-delta <= (-1 * threshold3) and net-capacity >= threshold1 and customers_waiting <= threshold2)
    ) [
      print (word "⏸️ " firm_ID " (Direct) → No change. cap=" ceiling capacity
             ", bookings=" ceiling bookings
             ", waiting=" ceiling customers_waiting)
    ]
  ]

  ;; === Step 2: React for Indirectly Connected Firms (2-hop neighbors) ===
  ask firms with [
    firm_ID != 1 and
    not link-neighbor? firm1 and
    any? link-neighbors with [link-neighbor? firm1]
  ] [
    let raw-delta ceiling (delta1 * 0.6)                    ;; Apply 40% decay
    let inferred-delta raw-delta
    let net-capacity (capacity - bookings)

    print (word firm_ID " (Indirect) sees ∆=" inferred-delta
          " | net-cap=" ceiling net-capacity
          " | waiting=" ceiling customers_waiting)

    if (inferred-delta >= threshold3) and
       (net-capacity <= threshold1) and
       (customers_waiting >= threshold2) [
      let change ceiling (customers_waiting * c1)
      set capacity capacity + change
      print (word "✅ " firm_ID " ↑ Indirect ↑ capacity=" ceiling capacity
            ", bookings=" ceiling bookings
            ", client_count=" ceiling (bookings + customers_waiting))
      set pre_decision_booking bookings
    ]

    if (inferred-delta <= (-1 * threshold3)) and
       (net-capacity >= threshold1) and
       (customers_waiting <= threshold2) [
      let change ceiling ((capacity - bookings) * c2)
      set capacity max (list (capacity - change) 0)
      print (word "⚠️ " firm_ID " ↓ Indirect ↓ capacity=" ceiling capacity
            ", bookings=" ceiling bookings
            ", client_count=" ceiling (bookings + customers_waiting))
      set pre_decision_booking bookings
    ]

    if not (
      (inferred-delta >= threshold3 and net-capacity <= threshold1 and customers_waiting >= threshold2) or
      (inferred-delta <= (-1 * threshold3) and net-capacity >= threshold1 and customers_waiting <= threshold2)
    ) [
      print (word "⏸️ " firm_ID " (Indirect) → No change. cap=" ceiling capacity
             ", bookings=" ceiling bookings
             ", waiting=" ceiling customers_waiting)
    ]
  ]

end


;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;   TICK 9      ;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to tick-9-network-response
    ;; === Set constants for decision-making ===
  let threshold1 0                ;; Threshold for net capacity
  let threshold2 0                ;; Threshold for waiting customers
  let threshold4 5                ;; Threshold for network-based decision
  let c1 0.5                      ;; % multiplier to increase capacity
  let c2 0.5                      ;; % multiplier to decrease capacity

  print ("=== 📊 Tick 9: Network-wide Performance Assessment ===")

  let firm1 one-of firms with [firm_ID = 1]
   ;; Get all firms sorted by firm-ID (for consistent execution order)
  let sorted-firms sort-by [[a b] -> [firm_ID] of a < [firm_ID] of b] firms
  foreach sorted-firms [f ->
    ask f [
      ;; grab only the firm neighbours
      let firm-neighs link-neighbors with [ breed = firms ]

      ;; if there are none, skip out of this ask and move on
      ifelse not any? firm-neighs [
        print (word firm_ID "⛔ has no competitors — skipping decision.")
      ] [
        ;;else .... do your decision code here
        let direct-neighbors firm-neighs
        let indirect-neighbors firms with [
          self != myself and
          not member? self direct-neighbors and
          any? link-neighbors with [member? self direct-neighbors]
        ]
        ;;== Identify direct and indirect neighbors

        let direct-ids [firm_ID] of direct-neighbors
        let indirect-ids [firm_ID] of indirect-neighbors
        print(word firm_ID " 🔗 Direct Neighbors: " direct-ids " | 🌐 Indirect Neighbors: " indirect-ids)

        ;; === Calculate and Print ∆bookings for Direct Neighbors ===
        let sum_direct 0
        if any? direct-neighbors [
          foreach (list direct-neighbors) [ dn ->
            ask dn [
              let delta bookings - pre_decision_booking
              print (word "📊 " firm_ID " → Direct Neighbor: " firm_ID ", bookings=" bookings ", pre-decision=" pre_decision_booking ", ∆=" delta)
            ]
          ]
          set sum_direct mean [bookings - pre_decision_booking] of direct-neighbors
        ]

        ;; === Calculate and Print ∆bookings for Indirect Neighbors ===
        let sum_indirect 0
        if any? indirect-neighbors [
          foreach (list indirect-neighbors) [ in ->
            ask in [
              let delta bookings - pre_decision_booking
              print (word "🌐 " firm_ID " → Indirect Neighbor: " firm_ID ", bookings=" bookings ", pre-decision=" pre_decision_booking ", ∆=" delta)
            ]
          ]
          set sum_indirect mean [bookings - pre_decision_booking] of indirect-neighbors
        ]

        ;;=== Calculate Perceived Performance ===
        let num_of_neighbours (count direct-neighbors + count indirect-neighbors)
        let raw-performance ceiling (((sum_direct * 0.95) + (sum_indirect * 0.90))/(num_of_neighbours))
        set perceived-performance raw-performance
        let net-capacity (capacity - bookings)

        ;; === Debug output ===
        print (word firm_ID
          " | ∆direct=" ceiling sum_direct
          ", ∆indirect=" ceiling sum_indirect
          ", raw=" ceiling raw-performance
          ", perceived=" perceived-performance
          ", net-capacity=" ceiling net-capacity
          ", waiting=" ceiling customers_waiting)

        ;; === Decision: Increase Capacity ===
        if (perceived-performance >= threshold4) and
        (net-capacity <= threshold1) and
        (customers_waiting >= threshold2) [
          let increase-by customers_waiting * c1
          set capacity ceiling (capacity + increase-by)
          if capacity > 75 [ set capacity 75 ]
          print (word "✅ " firm_ID " ↑ Increased capacity to " capacity)
          set pre_decision_booking bookings
        ]

        ;; === Decision: Decrease Capacity ===
        if (perceived-performance <= (-1 * threshold4)) and
        (net-capacity >= threshold1) and
        (customers_waiting <= threshold2) [
          let decrease-by ceiling ((capacity - bookings) * c2)
          set capacity ceiling max (list (capacity - decrease-by) 0)
          print (word "⚠️ " firm_ID " ↓ Reduced capacity to " capacity)
          set pre_decision_booking bookings
        ]

        ;; === No change if conditions are not met ===
        if not (
          (perceived-performance >= threshold4 and net-capacity <= threshold1 and customers_waiting >= threshold2) or
          (perceived-performance <= (-1 * threshold4) and net-capacity >= threshold1 and customers_waiting <= threshold2)
          ) [
          print (word "⏸️ " firm_ID " made no change. Conditions not met.")
        ]
      ]
    ]
  ]

end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; CSV Parse Line ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Splits a single line of text at commas, returning a list of items (strings).
to-report csv-parse-line [line]
  let parts []
  let current ""
  let length-line length line
  let index 0
  while [index < length-line] [
    let char item index line
    ifelse (char = ",") [
      set parts lput current parts
      set current ""
    ] [
      set current (word current char)
    ]
    set index (index + 1)
  ]
  ;; After the loop, add the final 'current' to parts
  set parts lput current parts
  report parts
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Import Firm Initialization ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to import-firm-initialization [filename]
  file-open filename

  ;; Skip header line
  let header file-read-line

  ;; Collect each non-empty row in a list
  let parsed-rows []
  while [not file-at-end?] [
    let line file-read-line
    if (line != "") [
      set parsed-rows lput (csv-parse-line line) parsed-rows
    ]
  ]
  file-close

  let min-lat  999999
  let max-lat -999999
  let min-lon  999999
  let max-lon -999999

  foreach parsed-rows [ row ->
    let lat read-from-string item 1 row
    let lon read-from-string item 2 row
    if (lat < min-lat) [ set min-lat lat ]
    if (lat > max-lat) [ set max-lat lat ]
    if (lon < min-lon) [ set min-lon lon ]
    if (lon > max-lon) [ set max-lon lon ]
  ]

  set firm-ids []

  ;; Create one firm turtle per row and assign columns
  create-firms length parsed-rows [
    let row (item who parsed-rows)

    ;; Assign attributes
    set firm_ID              (read-from-string item  0 row)
    set latitude             (read-from-string item  1 row)
    set longitude            (read-from-string item  2 row)
    set capacity             (read-from-string item  3 row)
    set overbook             (read-from-string item  4 row)
    set customers_count         (read-from-string item  5 row)
    set bookings             (read-from-string item  6 row)
    set customers_waiting    (read-from-string item  7 row)
    set pre_decision_booking (read-from-string item  8 row)
    set franchise_ID         (read-from-string item  9 row)
    set star_rating          (read-from-string item 10 row)
    set firm_age             (read-from-string item 11 row)
    set f_deviance_propensity  (read-from-string item 12 row)
    set firm_decision        (read-from-string item 13 row)
    set firm_status          (read-from-string item 14 row)

    ;; Normalize lat/lon to NetLogo world [-15, 15]
    let netlogo-min-x -15
    let netlogo-max-x  15
    let netlogo-min-y -15
    let netlogo-max-y  15

    let fraction-lat ( (latitude  - min-lat) / ((max-lat - min-lat) + 1e-6) )
    let fraction-lon ( (longitude - min-lon) / ((max-lon - min-lon) + 1e-6) )

    ;; Typically lat -> y, lon -> x
    let mapped-x netlogo-min-x + fraction-lon * (netlogo-max-x - netlogo-min-x)
    let mapped-y netlogo-min-y + fraction-lat * (netlogo-max-y - netlogo-min-y)

    setxy mapped-x mapped-y

    set shape "house"
    set color green
    set firm-ids lput firm_ID firm-ids
  ]
  print (word "firm-ids: " firm-ids)
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Import Customer Initialization ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to import-customers-initialization [filename]
  file-open filename
  ;; If your CSV has a header, skip it here
  let header file-read-line

  let customer-rows []
  while [not file-at-end?] [
    let line file-read-line
    if (line != "") [
      set customer-rows lput (csv-parse-line line) customer-rows
    ]
  ]
  file-close

  let min-lat  999999
  let max-lat -999999
  let min-lon  999999
  let max-lon -999999

  foreach customer-rows [ row ->
    let lat read-from-string item 1 row
    let lon read-from-string item 2 row
    if (lat < min-lat) [ set min-lat lat ]
    if (lat > max-lat) [ set max-lat lat ]
    if (lon < min-lon) [ set min-lon lon ]
    if (lon > max-lon) [ set max-lon lon ]
  ]

  foreach customer-rows [ row ->
    ;create-customers length customer-rows [
    create-customers 1 [
      ;let row (item who customer-rows)
      set customer_ID          (read-from-string item 0 row)
      set latitude             (read-from-string item 1 row)
      set longitude            (read-from-string item 2 row)
      set stickiness           (read-from-string item 3 row)
      set random_number 0
      set current_firm         (read-from-string item 4 row)
      set previous_firm        (read-from-string item 5 row)
      set decision             item 6 row
      set c_deviance_propensity  (read-from-string item 7 row)
      set utility 0

      ;; Normalize lat/lon to NetLogo world [-15, 15]
      let netlogo-min-x -15
      let netlogo-max-x  15
      let netlogo-min-y -15
      let netlogo-max-y  15

      let fraction-lat ( (latitude  - min-lat) / ((max-lat - min-lat) + 1e-6) )
      let fraction-lon ( (longitude - min-lon) / ((max-lon - min-lon) + 1e-6) )

      ;; Typically lat -> y, lon -> x
      let mapped-x netlogo-min-x + fraction-lon * (netlogo-max-x - netlogo-min-x)
      let mapped-y netlogo-min-y + fraction-lat * (netlogo-max-y - netlogo-min-y)

      setxy mapped-x mapped-y

      set shape "person"
      set color pink
    ]
  ]
end



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Request service form a new firm;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


to request_service_from_new_firm
  ask my-links [die] ;; any existing link with the customer will die before running the rest of this function
  print(word "requesting service from a new firm")
  let max_utility -100000
  let best_firm nobody

  ;; Find the firm with maximum utility (distance + star-rating),
  ;; ignoring the current_firm
  ask firms [
    if (distance myself < radius) and (firm_ID != [current_firm] of myself) [
      let travel_distance distance myself
      let total_utility (5 * ((radius - travel_distance) / radius)) + star_rating

      if (total_utility > max_utility) [
        set max_utility total_utility
        set best_firm self
      ]
    ]
  ]

  ;; 2) Only proceed if we found a firm with utility above some threshold
  ifelse (max_utility > threshold) and (best_firm != nobody) [
    ifelse ([bookings] of best_firm < [capacity] of best_firm)
    [
      ;; (A) If the best_firm has capacity:
      set previous_firm current_firm
      set current_firm [firm_ID] of best_firm
      set decision "move"

      ;; Link to the new firm
      create-link-with best_firm

      ;; Update the firm's counters
      ask best_firm [
        set bookings (bookings + 1)
        set customers_count (customers_count + 1)
      ]
    ]
    [
      ;; (B) Otherwise, the best_firm is full => we wait
      set previous_firm [firm_ID] of best_firm
      set current_firm 0
      set decision "wait"

      ask best_firm [
        set customers_waiting (customers_waiting + 1)
        set customers_count (customers_count + 1)
      ]
    ]
  ]
  [
    set decision "wait"
    set current_firm 0
    set previous_firm 0
  ]
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Request service form previous firm;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to request_service_from_previous_firm
  print(word "requesting service from previous firm")
  let preferred_firm nobody
  set preferred_firm one-of firms with [(who + 1) = [previous_firm] of myself]
  ifelse ([bookings] of preferred_firm < [capacity] of preferred_firm) [
    set current_firm previous_firm
    set previous_firm 0
    ask preferred_firm [
      set bookings (bookings + 1)
      set customers_count (customers_count + 1)
    ]
    set decision "move"
    create-link-with preferred_firm
  ][
    set previous_firm 0
    set decision "wait"
  ]

end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Request service form current firm;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to request_service_from_current_firm
  print(word "requesting service from current firm")
  let preferred_firm nobody
  set preferred_firm one-of firms with [(who + 1) = [current_firm] of myself]
  print (word "This is printing the preferred firm of the client:" preferred_firm)
  print (word "This is the current firm: " current_firm)
  print (word "This is the current who: " who)
  ifelse ([bookings] of preferred_firm < [capacity] of preferred_firm) [
   set decision "stay"
   ask preferred_firm [
     set bookings (bookings + 1)
     set customers_count (customers_count + 1)
    ]
  ]
  [
    set decision "move"
    set previous_firm current_firm
    set current_firm 0
    ask link-with preferred_firm [die]
    ask preferred_firm [
      set customers_waiting (customers_waiting + 1)
      set customers_count (customers_count + 1)
    ]
  ]

end


;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Save Firms Output ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;

to save_firms_output
  let firms_sts "firms-output.csv"
  if ticks = 0 [
    file-open firms_sts
    file-print "tick,firm_ID,capacity,overbook,customers_count,bookings,customers_waiting,pre_decision_booking,franchise_ID,star_rating,firm_age,f_deviance_propensity,firm_decision,firm_status"
    file-close
  ]
  file-open firms_sts
  ask firms [
    file-print (word ticks "," firm_ID "," capacity "," overbook "," customers_count "," bookings "," customers_waiting "," pre_decision_booking "," franchise_ID "," star_rating "," firm_age "," f_deviance_propensity "," firm_decision "," firm_status)
  ]
  file-close
end


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Save Customers Output ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to save_customers_output
  let customers_sts "customers-output.csv"
  if ticks = 0 [
    file-open customers_sts
    file-print "tick,customer_ID,latitude,longitude,stickiness,random_number,current_firm,previous_firm,decision,c_deviance_propensity"
    file-close
  ]
  file-open customers_sts
  ask customers [
    file-print (word ticks "," customer_ID "," latitude "," longitude "," stickiness "," random_number "," current_firm "," previous_firm "," decision "," c_deviance_propensity)
  ]
  file-close
end


;;;;;;;;;;;;;;;;;;;;
;;; Go Procedure ;;;
;;;;;;;;;;;;;;;;;;;;

to go
  ;; 1) Reset firm counters each tick
  ;ask firms [
    ;ask my-links [
      ;let other-turtle ifelse-value (end1 = myself) [ end2 ] [end1]
      ;if other-turtle != nobody and [breed] of other-turtle = firms [
        ;die
      ;]
    ;]
  ;]

  ask firms [
    set bookings 0
    set customers_count 0
    set customers_waiting 0
  ]

  ;; Just showing the sorted list for debugging
  ;show sort-on [who] customers

  ;; 2) Process customers in ascending order of 'who'
  ;foreach sort-on [who] customers [ the-customer ->
    ;ask the-customer [
  ask customers [
      print (word "---------------------------------")
      print (word "Customer ID: " customer_ID)
      print (word "Who in NetLogo: " who)
      print (word "Stickiness: " stickiness)
      print (word "Previous Firm: " previous_firm)
      print (word "Current Firm: " current_firm)
      set random_number 0 ;; will set the random number to zero at the beginning of each tick

      ifelse (current_firm = 0 and previous_firm = 0)
      [
        ;; Case 1: No current, no previous => get new firm
        request_service_from_new_firm
      ]
      [
        ifelse (current_firm = 0 and previous_firm != 0)
        [
          ;; Case 2: No current, but have previous => go to previous
          request_service_from_previous_firm
        ]
        [
          ifelse (current_firm != 0 and previous_firm = 0)
          [
            ;; Case 3: Have current, no previous => random check
            set random_number random 100
            print (word "random number: " random_number)
            ifelse (random_number > stickiness)
            [
              ;ask link-neighbors [ die ]
              ;ask my-links [die]
              request_service_from_new_firm
            ]
            [
              request_service_from_current_firm
            ]
          ]
          [
            ;; Case 4: Have current AND have previous => random check
            set random_number random 100
            print (word "random number: " random_number)
            ifelse (random_number > stickiness)
            [
              ;ask link-neighbors [ die ]
              ;ask my-links [die]
              request_service_from_new_firm
            ]
            [
              request_service_from_current_firm
            ]
          ]
        ]
      ]
    ]

  ;;focal firm (firm1) decision making
  if ticks = 1 [
    tick-1-focal-decision
    ask firms [
      set pre_decision_booking bookings
    ]
  ]

  if ticks = 5 [
    tick-5-react-to-firm1
  ]

  if ticks = 9 [
    tick-9-network-response
  ]

  if ticks > 9 [
    let cycle-tick (ticks - 10) mod 4
    if cycle-tick = 3 [
      tick-9-network-response
    ]
  ]

  ;; Save firms and customers output
  save_firms_output
  save_customers_output
  tick
end




to go-once
  go
end
@#$#@#$#@
GRAPHICS-WINDOW
176
10
977
812
-1
-1
13.0
1
10
1
1
1
0
1
1
1
-30
30
-30
30
0
0
1
ticks
30.0

BUTTON
39
32
127
67
setup
setup
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
37
85
126
127
NIL
go
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
36
144
129
189
NIL
go-once
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

@#$#@#$#@
## WHAT IS IT?

(a general understanding of what the model is trying to show or explain)

## HOW IT WORKS

(what rules the agents use to create the overall behavior of the model)

## HOW TO USE IT

(how to use the model, including a description of each of the items in the Interface tab)

## THINGS TO NOTICE

(suggested things for the user to notice while running the model)

## THINGS TO TRY

(suggested things for the user to try to do (move sliders, switches, etc.) with the model)

## EXTENDING THE MODEL

(suggested things to add or change in the Code tab to make the model more complicated, detailed, accurate, etc.)

## NETLOGO FEATURES

(interesting or unusual features of NetLogo that the model uses, particularly in the Code tab; or where workarounds were needed for missing features)

## RELATED MODELS

(models in the NetLogo Models Library and elsewhere which are of related interest)

## CREDITS AND REFERENCES

(a reference to the model's URL on the web if it has one, as well as any other necessary credits, citations, and links)
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

airplane
true
0
Polygon -7500403 true true 150 0 135 15 120 60 120 105 15 165 15 195 120 180 135 240 105 270 120 285 150 270 180 285 210 270 165 240 180 180 285 195 285 165 180 105 180 60 165 15

arrow
true
0
Polygon -7500403 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

box
false
0
Polygon -7500403 true true 150 285 285 225 285 75 150 135
Polygon -7500403 true true 150 135 15 75 150 15 285 75
Polygon -7500403 true true 15 75 15 225 150 285 150 135
Line -16777216 false 150 285 150 135
Line -16777216 false 150 135 15 75
Line -16777216 false 150 135 285 75

bug
true
0
Circle -7500403 true true 96 182 108
Circle -7500403 true true 110 127 80
Circle -7500403 true true 110 75 80
Line -7500403 true 150 100 80 30
Line -7500403 true 150 100 220 30

butterfly
true
0
Polygon -7500403 true true 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -7500403 true true 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -7500403 true true 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -7500403 true true 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -16777216 true false 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60

car
false
0
Polygon -7500403 true true 300 180 279 164 261 144 240 135 226 132 213 106 203 84 185 63 159 50 135 50 75 60 0 150 0 165 0 225 300 225 300 180
Circle -16777216 true false 180 180 90
Circle -16777216 true false 30 180 90
Polygon -16777216 true false 162 80 132 78 134 135 209 135 194 105 189 96 180 89
Circle -7500403 true true 47 195 58
Circle -7500403 true true 195 195 58

circle
false
0
Circle -7500403 true true 0 0 300

circle 2
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240

cow
false
0
Polygon -7500403 true true 200 193 197 249 179 249 177 196 166 187 140 189 93 191 78 179 72 211 49 209 48 181 37 149 25 120 25 89 45 72 103 84 179 75 198 76 252 64 272 81 293 103 285 121 255 121 242 118 224 167
Polygon -7500403 true true 73 210 86 251 62 249 48 208
Polygon -7500403 true true 25 114 16 195 9 204 23 213 25 200 39 123

cylinder
false
0
Circle -7500403 true true 0 0 300

dot
false
0
Circle -7500403 true true 90 90 120

face happy
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 255 90 239 62 213 47 191 67 179 90 203 109 218 150 225 192 218 210 203 227 181 251 194 236 217 212 240

face neutral
false
0
Circle -7500403 true true 8 7 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Rectangle -16777216 true false 60 195 240 225

face sad
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 168 90 184 62 210 47 232 67 244 90 220 109 205 150 198 192 205 210 220 227 242 251 229 236 206 212 183

fish
false
0
Polygon -1 true false 44 131 21 87 15 86 0 120 15 150 0 180 13 214 20 212 45 166
Polygon -1 true false 135 195 119 235 95 218 76 210 46 204 60 165
Polygon -1 true false 75 45 83 77 71 103 86 114 166 78 135 60
Polygon -7500403 true true 30 136 151 77 226 81 280 119 292 146 292 160 287 170 270 195 195 210 151 212 30 166
Circle -16777216 true false 215 106 30

flag
false
0
Rectangle -7500403 true true 60 15 75 300
Polygon -7500403 true true 90 150 270 90 90 30
Line -7500403 true 75 135 90 135
Line -7500403 true 75 45 90 45

flower
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Circle -7500403 true true 85 132 38
Circle -7500403 true true 130 147 38
Circle -7500403 true true 192 85 38
Circle -7500403 true true 85 40 38
Circle -7500403 true true 177 40 38
Circle -7500403 true true 177 132 38
Circle -7500403 true true 70 85 38
Circle -7500403 true true 130 25 38
Circle -7500403 true true 96 51 108
Circle -16777216 true false 113 68 74
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

leaf
false
0
Polygon -7500403 true true 150 210 135 195 120 210 60 210 30 195 60 180 60 165 15 135 30 120 15 105 40 104 45 90 60 90 90 105 105 120 120 120 105 60 120 60 135 30 150 15 165 30 180 60 195 60 180 120 195 120 210 105 240 90 255 90 263 104 285 105 270 120 285 135 240 165 240 180 270 195 240 210 180 210 165 195
Polygon -7500403 true true 135 195 135 240 120 255 105 255 105 285 135 285 165 240 165 195

line
true
0
Line -7500403 true 150 0 150 300

line half
true
0
Line -7500403 true 150 0 150 150

pentagon
false
0
Polygon -7500403 true true 150 15 15 120 60 285 240 285 285 120

person
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105

plant
false
0
Rectangle -7500403 true true 135 90 165 300
Polygon -7500403 true true 135 255 90 210 45 195 75 255 135 285
Polygon -7500403 true true 165 255 210 210 255 195 225 255 165 285
Polygon -7500403 true true 135 180 90 135 45 120 75 180 135 210
Polygon -7500403 true true 165 180 165 210 225 180 255 120 210 135
Polygon -7500403 true true 135 105 90 60 45 45 75 105 135 135
Polygon -7500403 true true 165 105 165 135 225 105 255 45 210 60
Polygon -7500403 true true 135 90 120 45 150 15 180 45 165 90

sheep
false
15
Circle -1 true true 203 65 88
Circle -1 true true 70 65 162
Circle -1 true true 150 105 120
Polygon -7500403 true false 218 120 240 165 255 165 278 120
Circle -7500403 true false 214 72 67
Rectangle -1 true true 164 223 179 298
Polygon -1 true true 45 285 30 285 30 240 15 195 45 210
Circle -1 true true 3 83 150
Rectangle -1 true true 65 221 80 296
Polygon -1 true true 195 285 210 285 210 240 240 210 195 210
Polygon -7500403 true false 276 85 285 105 302 99 294 83
Polygon -7500403 true false 219 85 210 105 193 99 201 83

square
false
0
Rectangle -7500403 true true 30 30 270 270

square 2
false
0
Rectangle -7500403 true true 30 30 270 270
Rectangle -16777216 true false 60 60 240 240

star
false
0
Polygon -7500403 true true 151 1 185 108 298 108 207 175 242 282 151 216 59 282 94 175 3 108 116 108

target
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Circle -7500403 true true 60 60 180
Circle -16777216 true false 90 90 120
Circle -7500403 true true 120 120 60

tree
false
0
Circle -7500403 true true 118 3 94
Rectangle -6459832 true false 120 195 180 300
Circle -7500403 true true 65 21 108
Circle -7500403 true true 116 41 127
Circle -7500403 true true 45 90 120
Circle -7500403 true true 104 74 152

triangle
false
0
Polygon -7500403 true true 150 30 15 255 285 255

triangle 2
false
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -16777216 true false 151 99 225 223 75 224

truck
false
0
Rectangle -7500403 true true 4 45 195 187
Polygon -7500403 true true 296 193 296 150 259 134 244 104 208 104 207 194
Rectangle -1 true false 195 60 195 105
Polygon -16777216 true false 238 112 252 141 219 141 218 112
Circle -16777216 true false 234 174 42
Rectangle -7500403 true true 181 185 214 194
Circle -16777216 true false 144 174 42
Circle -16777216 true false 24 174 42
Circle -7500403 false true 24 174 42
Circle -7500403 false true 144 174 42
Circle -7500403 false true 234 174 42

turtle
true
0
Polygon -10899396 true false 215 204 240 233 246 254 228 266 215 252 193 210
Polygon -10899396 true false 195 90 225 75 245 75 260 89 269 108 261 124 240 105 225 105 210 105
Polygon -10899396 true false 105 90 75 75 55 75 40 89 31 108 39 124 60 105 75 105 90 105
Polygon -10899396 true false 132 85 134 64 107 51 108 17 150 2 192 18 192 52 169 65 172 87
Polygon -10899396 true false 85 204 60 233 54 254 72 266 85 252 107 210
Polygon -7500403 true true 119 75 179 75 209 101 224 135 220 225 175 261 128 261 81 224 74 135 88 99

wheel
false
0
Circle -7500403 true true 3 3 294
Circle -16777216 true false 30 30 240
Line -7500403 true 150 285 150 15
Line -7500403 true 15 150 285 150
Circle -7500403 true true 120 120 60
Line -7500403 true 216 40 79 269
Line -7500403 true 40 84 269 221
Line -7500403 true 40 216 269 79
Line -7500403 true 84 40 221 269

wolf
false
0
Polygon -16777216 true false 253 133 245 131 245 133
Polygon -7500403 true true 2 194 13 197 30 191 38 193 38 205 20 226 20 257 27 265 38 266 40 260 31 253 31 230 60 206 68 198 75 209 66 228 65 243 82 261 84 268 100 267 103 261 77 239 79 231 100 207 98 196 119 201 143 202 160 195 166 210 172 213 173 238 167 251 160 248 154 265 169 264 178 247 186 240 198 260 200 271 217 271 219 262 207 258 195 230 192 198 210 184 227 164 242 144 259 145 284 151 277 141 293 140 299 134 297 127 273 119 270 105
Polygon -7500403 true true -1 195 14 180 36 166 40 153 53 140 82 131 134 133 159 126 188 115 227 108 236 102 238 98 268 86 269 92 281 87 269 103 269 113

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270
@#$#@#$#@
NetLogo 6.4.0
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180
@#$#@#$#@
0
@#$#@#$#@
