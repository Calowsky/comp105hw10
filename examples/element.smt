(class Element
   [subclass-of Object]
   [ivars tag atts]

   (class-method new: (tagsym) ((self new) init: tagsym))
   (method init: (tagsym) 
      (set tag tagsym) (set atts (Dictionary new)) self)
   (method print () 
      ('< print)
      (tag print) 
      (atts associationsDo:
         [block (entry) 
            (space print)
            ((entry key) print)
            ('=" print) 
            ((entry value) print)
            ('" print)])
         ('/> print))
   (method attribute:put: (att v) (atts at:put: att v) self)
)

(val helloelem (Element new: 'hello))
(helloelem attribute:put: 'name 'Josh)
(helloelem attribute:put: 'friend 'Yassin)
