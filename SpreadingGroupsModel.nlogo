; adapted from models library by Lada Adamic (see copyright below)
; for the purposes of SI708/CSCS608
; also now contains major improvements by Eytan Bakshy
extensions [nw]
globals
[
  new-node  ;; the last node we created
  degrees   ;; this is an array that contains each node in
            ;; proportion to its degree
  time
  num-infected
  previous-num-infected
  tree-mode?
  spreading-group

]

turtles-own
[
  infected?        ;; true if agent has been infected
]

;;;;;;;;;;;;;;;;;;;;;;;;
;;; Setup Procedures ;;;
;;;;;;;;;;;;;;;;;;;;;;;;

to setup
  ;; (for this model to work with NetLogo's new plotting features,
  ;; __clear-all-and-reset-ticks should be replaced with clear-all at
  ;; the beginning of your setup procedure and reset-ticks at the end
  ;; of the procedure.)
  __clear-all-and-reset-ticks
  set tree-mode? false
  set-default-shape turtles "circle"
  set degrees []   ;; initialize the array to be empty
  ;; make the initial network of two turtles and an edge


  make-node ;; add the very first node

  let first-node new-node  ;; remember that its the first node

  ;; the following few lines create a cycle of length 5
  ;; this is just an arbitrary way to start a network

  let prev-node new-node
  repeat 4 [
    make-node ;; second node
    make-edge new-node prev-node ;; make the edge
    set degrees lput prev-node degrees
    set degrees lput new-node degrees
    set prev-node new-node
  ]
  make-edge new-node first-node

  set time 0
  set num-infected 0
  set previous-num-infected 0

  repeat (num-nodes / 2) [create-preferential-net]


  set spreading-group n-of (num-nodes * spreading-group-percent / 100 )turtles
  ask spreading-group [set color orange set size 1]
  repeat spreadingGroupRepet [create-spreading-group]

  repeat (num-nodes / 2 - 4) [create-preferential-net]
display
end

to create-preferential-net
    no-display
    make-node  ;; add one new node

    ;; it's going to have m edges
    repeat m [
      let partner find-partner new-node      ;; find a partner for the new node
      ask partner [set color gray + 2]    ;; set color of partner to gray
      make-edge new-node partner     ;; connect it to the partner we picked before
    ]
    if layout? [ do-layout ]
    if plot? [ do-plotting ]
end


to create-spreading-group
  ask spreading-group
    [let first-node [who] of one-of spreading-group
      let second-node [who] of one-of spreading-group
      if (first-node != second-node)
        [if not is-link? link first-node second-node [
          ask turtle first-node [
            set size 2
            create-link-with turtle second-node[
              set color orange
              set thickness 0.5]
          ]
        ]
        ]
    ]
  repeat 5 [layout-spring (spreading-group) (links with [color = red]) 5 0.5 0.5]
end





;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Runtime Procedures ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

; reset diffusion simulation

to reinfect-once
  clear-plot
  reset-ticks
  set num-infected num-infected-init
  set time 0
  set previous-num-infected 0
  ask turtles [set color grey
    set infected? false
    set size 1]

  if (reinfect-type = "random")
  [ask n-of num-infected-init turtles
    [set color red
      set size 2
      set infected? true
    ]
  ]

  if (reinfect-type = "group")
  [ask n-of num-infected-init spreading-group
    [set color red
      set size 2
      set infected? true
    ]
  ]

  if (reinfect-type = "eigenval-centrality")
  [ask max-n-of num-infected-init turtles [nw:eigenvector-centrality]
    [set color red
      set size 2
      set infected? true
    ]
  ]

  if (reinfect-type = "PageRank-Centrality")
    [ask max-n-of num-infected-init turtles [ nw:page-rank]
      [set color red
        set size 2
        set infected? true
      ]
    ]



end

;; toggle infection tree mode
;; when tree-mode is on, only links responsible for contagion and infected nodes
;; are displayed. tree-mode also affects layout
to toggle-tree
  ifelse tree-mode?
  [
    ask turtles with [not infected?] [show-turtle]
    ask links with [color != red ] [show-link]
    set tree-mode? false
  ]
  [
    ask turtles with [not infected?] [hide-turtle]
    ask links with [color != red] [hide-link]
    set tree-mode? true
  ]
end

to spread
  ;; infection can't take place while in infection tree mode
  ;; or if every agent has already been infected
  if all? turtles [infected?]
    [stop]

  ask turtles with [ infected? = true ]
  [let mysize [size] of turtle who
    let prob 0
    ;; infect neighbors
    ask link-neighbors with [infected? = false]
    [ifelse (Retention-loss = true)
      [set prob p * mysize]
      [set prob p]
    if ( random-float 1 <= prob)  ;; infect with probability p or with prob p * size
      [ set infected? true
        show-turtle
        set color red
      ;; color the link with the node doing the infection
        ask link-with myself [show-link]
        ;; increment the total number of infected agents
        set num-infected num-infected + 1
      ]
    ]
  ]

    ;; resize node so that the area is proportional to the current number of infections


  let tmp count turtles with [infected? = true]
  ifelse (tmp = previous-num-infected)
  [stop]
  [set  previous-num-infected tmp]

  forgetness
  do-plotting

  tick
end

;; used for creating a new node
to make-node
  create-turtles 1  ;; don't know what this is - lada
  [
    set color gray + 2
    set size 0.5
    set infected? false
    set new-node self ;; set the new-node global
  ]
end



;; Find a node to attach to
;; the degree of a node
;; 0 < gamma < 1 means gamma*100 percent of the
;; time attach randomly, rest of the time attach
;; preferentially

to-report find-partner [node1]
  ;; set a local variable called ispref that
  ;; determines if this link is going to be
  ;; preferential of not
  let ispref (random-float 1 >= gamma)

  ;; initialize partner to be the node itself
  ;; this will have to be changed
  let partner node1

  ;; if preferential attachment then choose
  ;; from our degrees array
  ;; otherwise chose one of the turtles at random
  ifelse ispref
  [
    set partner one-of degrees
   ]
   [
     set partner one-of turtles
     ]

   ;; but need to check that partner chosen isn't
   ;; the node itself and also isn't a node that
   ;; our node is already connected to
   ;; if this is the case, it will try another
   ;; partner and try again
  let checkit true
  while [checkit] [
    ask partner [
      ifelse ((link-neighbor? node1) or (partner = node1))
        [
          ifelse ispref
          [
            set partner one-of degrees
           ]
           [
             set partner one-of turtles
           ]
            set checkit true
         ]
         [
           set checkit false
         ]
       ]
    ]
  report partner
end

;;;;;;;;;;;;;;;;;;;;;;;
;;; Edge Operations ;;;
;;;;;;;;;;;;;;;;;;;;;;;

;; connects the two turtles
to make-edge [node1 node2]
  ask node1 [
    ifelse (node1 = node2)
    [
      show "error: self-loop attempted"
    ]
    [
      create-link-with node2 [ set color grey + 2 ]
     ;; position the new node near its partner
      setxy ([xcor] of node2) ([ycor] of node2)
      rt random 360
      fd 8
      set degrees lput node1 degrees
     set degrees lput node2 degrees
     ]
  ]
end


;;;;;;;;;;;;;;;;
;;; Plotting ;;;
;;;;;;;;;;;;;;;;

to do-plotting
     ;; plot the number of infected individuals at each step
     set-current-plot "Number infected"
     set-current-plot-pen "inf"

     plotxy ticks num-infected
end

;;;;;;;;;;;;;;
;;; Layout ;;;
;;;;;;;;;;;;;;
;; resize-turtles, change back and forth from size based on degree to a size of 1

;; spring layout of infection tree while in tree mode
;; otherwise, layout all nodes and links
to do-layout
  ifelse tree-mode?
    [repeat 5 [layout-spring (spreading-group) (links) 5 0.5 0.5]]
    [repeat 5 [layout-spring turtles links 0.2 4 3]]
  display
end

to forgetness
  ask turtles
    [set size size / size-reduction
      if size < 0.1 [set infected? false]
    ]


end

; *** NetLogo 3.1.3 Model Copyright Notice ***
;
; Copyright 2005 by Uri Wilensky.  All rights reserved.
;
; Permission to use, modify or redistribute this model is hereby granted,
; provided that both of the following requirements are followed:
; a) this copyright notice is included.
; b) this model will not be redistributed for profit without permission
;    from Uri Wilensky.
; Contact Uri Wilensky for appropriate licenses for redistribution for
; profit.
;
; To refer to this model in academic publications, please use:
; Wilensky, U. (2005).  NetLogo Preferential Attachment model.
; http://ccl.northwestern.edu/netlogo/models/PreferentialAttachment.
; Center for Connected Learning and Computer-Based Modeling,
; Northwestern University, Evanston, IL.
;
; In other publications, please use:
; Copyright 2005 Uri Wilensky.  All rights reserved.
; See http://ccl.northwestern.edu/netlogo/models/PreferentialAttachment
; for terms of use.
;
; *** End of NetLogo 3.1.3 Model Copyright Notice ***
@#$#@#$#@
GRAPHICS-WINDOW
340
10
730
421
45
45
4.18
1
10
1
1
1
0
0
0
1
-45
45
-45
45
1
1
1
ticks
30.0

BUTTON
6
25
72
58
NIL
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
225
141
326
174
spread
spread
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
119
141
219
174
spread once
spread
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SWITCH
176
26
266
59
plot?
plot?
0
1
-1000

SWITCH
81
26
171
59
layout?
layout?
0
1
-1000

MONITOR
284
215
334
260
time
ticks
3
1
11

BUTTON
7
102
109
135
layout
do-layout
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SLIDER
153
64
325
97
m
m
1
5
2
1
1
NIL
HORIZONTAL

SLIDER
8
176
152
209
num-nodes
num-nodes
5
10000
200
1
1
NIL
HORIZONTAL

PLOT
8
213
280
415
Number infected
time
n
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"inf" 1.0 0 -2674135 true "" ""

SLIDER
10
65
145
98
p
p
0
1
0.11
0.01
1
NIL
HORIZONTAL

SLIDER
155
176
300
209
gamma
gamma
0
1
0.5
0.01
1
NIL
HORIZONTAL

MONITOR
1074
19
1147
72
nodes
count turtles
3
1
13

BUTTON
8
140
106
173
reinfect one
reinfect-once
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
119
101
262
134
toggle infection tree
toggle-tree
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
1735
27
1858
60
crt SpreadGroup
create-spreading-group
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SLIDER
772
73
944
106
num-infected-init
num-infected-init
0
50
5
1
1
NIL
HORIZONTAL

CHOOSER
31
472
212
517
reinfect-type
reinfect-type
"random" "group" "eigenval-centrality" "PageRank-Centrality"
0

MONITOR
977
19
1065
72
Group Size
count spreading-group
17
1
13

MONITOR
276
447
368
500
net-infected
num-infected  - num-infected-init
17
1
13

SLIDER
776
121
948
154
spreadingGroupRepet
spreadingGroupRepet
0
10
2
1
1
NIL
HORIZONTAL

SWITCH
776
216
911
249
Retention-loss
Retention-loss
0
1
-1000

SLIDER
777
167
994
200
spreading-group-percent
spreading-group-percent
0
5
5
0.05
1
NIL
HORIZONTAL

SLIDER
946
218
1118
251
size-reduction
size-reduction
1
10
1.5
0.25
1
NIL
HORIZONTAL

@#$#@#$#@
## WHAT IS IT?

Based on this Lada and Eytan's modification of the default BA model that comes with the NetLogo There is an added "Spreading Group".
The initial spread can be through random nodes, the spreading group, or the highest eigenvalue centrality nodes.

In addition, the retention loss is modified by the slider "size-reduction", the density of the spreafing group modified by the slider "spreadingGroupRepet" and its size modified by the slider "spreading-group-percent".



## HOW IT WORKS

The model starts with two nodes connected by an edge.
At each step, a new node is added.  after n/2 nodes added, a step of formation of the spreading gorup is performed, followed by another n/2 steps of preferential attachment.


## HOW TO USE IT

The NUM-NODES slider controls the size of the network.

The GAMMA parameter determines whether the attachment is preferential (gamma = 0
means entirely preferential, gamma = 1 means entirely random).

Choose num-nodes and gamma and press SETUP.

Adjust the p value to determine the infectiousness of the spreading agent.

To re-infect one will infect a single individual while keeping the same topology - press "reinfect-one".

Choose the "reinfect-type"

Chose the retention loss on/off as well as its speed.

Now to allow the disease to spread, you can advance on time step at a time (each infected node will infect each of its neighbors with probability p) with the "spread once" button. To let the disease run its full course, you can click the "spread" button.

## THINGS TO NOTICE

Compare the spread by random k nodes, random k nodes from the spreading group, k nodes of the highest eigenvalues.


## RELATED MODELS

See other models in the Networks section of the Models Library, such as Giant Component.

See also Network Example, in the Code Examples section.

## CREDITS AND REFERENCES

This model is based on:
Albert-Laszlo Barabasi. Linked: The New Science of Networks, Perseus Publishing, Cambridge, Massachusetts, pages 79-92.

For a more technical treatment, see:
Albert-Laszlo Barabasi & Reka Albert. Emergence of Scaling in Random Networks, Science, Vol 286, Issue 5439, 15 October 1999, pages 509-512.

Barabasi's webpage has additional information at: http://www.nd.edu/~alb/

The layout algorithm is based on the Fruchterman-Reingold layout algorithm.  More information about this algorithm can be obtained at: http://citeseer.ist.psu.edu/fruchterman91graph.html.

For a model similar to the one described in the first extension, please consult:
W. Brian Arthur, "Urban Systems and Historical Path-Dependence", Chapt. 4 in Urban systems and Infrastructure, J. Ausubel and R. Herman (eds.), National Academy of Sciences, Washington, D.C., 1988.

To refer to this model in academic publications, please use:  Wilensky, U. (2005).  NetLogo Preferential Attachment model.  http://ccl.northwestern.edu/netlogo/models/PreferentialAttachment.  Center for Connected Learning and Computer-Based Modeling, Northwestern University, Evanston, IL.

In other publications, please use:  Copyright 2005 Uri Wilensky.  All rights reserved.  See http://ccl.northwestern.edu/netlogo/models/PreferentialAttachment for terms of use.

Modified by Lada Adamic 2007

modified by Alon Sela 2016.
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

link
true
0
Line -7500403 true 150 0 150 300

link direction
true
0
Line -7500403 true 150 150 30 225
Line -7500403 true 150 150 270 225

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

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270

@#$#@#$#@
NetLogo 5.2.1
@#$#@#$#@
set layout? false
setup repeat 175 [ go ]
repeat 35 [ layout ]
@#$#@#$#@
@#$#@#$#@
<experiments>
  <experiment name="experiment1" repetitions="5" runMetricsEveryStep="true">
    <setup>setup
reinfect-once</setup>
    <go>spread</go>
    <timeLimit steps="200"/>
    <metric>num-infected</metric>
    <enumeratedValueSet variable="num-nodes">
      <value value="10000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reinfect-type">
      <value value="&quot;group&quot;"/>
      <value value="&quot;random&quot;"/>
      <value value="&quot;eigenval-centrality&quot;"/>
      <value value="&quot;PageRank-Centrality&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p">
      <value value="0.01"/>
      <value value="0.05"/>
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="layout?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="num-infected-init">
      <value value="15"/>
      <value value="25"/>
      <value value="35"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="spreadingGroupRepet">
      <value value="5"/>
      <value value="10"/>
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="spreading-group-percent">
      <value value="1"/>
      <value value="3"/>
      <value value="5"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment" repetitions="1" runMetricsEveryStep="true">
    <setup>setup
spread</setup>
    <go>spread</go>
    <timeLimit steps="100"/>
    <metric>count turtles with [infected? = true]</metric>
    <enumeratedValueSet variable="num-nodes">
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="plot?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="spreading-group-percent">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="num-infected-init">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="m">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="gamma">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reinfect-type">
      <value value="&quot;random&quot;"/>
      <value value="&quot;group&quot;"/>
      <value value="&quot;PageRank-Centrality&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Retention-loss">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="spreadingGroupRepet">
      <value value="5"/>
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="layout?">
      <value value="false"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment2" repetitions="5" runMetricsEveryStep="true">
    <setup>setup
reinfect-once</setup>
    <go>spread</go>
    <timeLimit steps="200"/>
    <metric>num-infected</metric>
    <enumeratedValueSet variable="num-nodes">
      <value value="10000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="reinfect-type">
      <value value="&quot;group&quot;"/>
      <value value="&quot;random&quot;"/>
      <value value="&quot;eigenval-centrality&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p">
      <value value="0.01"/>
      <value value="0.05"/>
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="layout?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="num-infected-init">
      <value value="15"/>
      <value value="25"/>
      <value value="35"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="spreadingGroupRepet">
      <value value="5"/>
      <value value="10"/>
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="spreading-group-percent">
      <value value="1"/>
      <value value="3"/>
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="size-reduction">
      <value value="1.5"/>
      <value value="3"/>
      <value value="4.5"/>
    </enumeratedValueSet>
  </experiment>
</experiments>
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
