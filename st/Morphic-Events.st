"Click events are processed like this:
- HandMorph queues events passed to it from the 'hardware interrupt' listeners.
- Each hand gets called with #processEvents.
- First each event is sent to #handleListenEvent:, this is a general means
  to capture all events globally.
- If there's a concept of 'focus' with a hand focused on some element, send
  theFocus handleFocusEvent: theEvent and done.
- Without a focus, the event is dispatched to the world:
  self world processEvent: theEvent.
- To allow for pluggable strategies, this uses a separate, pluggable dispatcher.
  - Morph>>processEvent: calls
      self processEvent: e using: self defaultEventDispatcher.
  - Morph>>processEvent:using: calls self rejectsEvent: anEvent, but if it is
    not rejected then it calls dispatcher dispatchEvent: anEvent with: self.
  - The dispatcher is then responsible for defining the flow.
- The default dispatcher is similar to the web (or rather the web is similar to
  Morphic!)
  - It splits on type, calling eg. #dispatchClick:with:
  - Which checks the point is inside fullBounds (^ #rejected otherwise).
  - If it is inside, aMorph handlerForClick: anEvent, if that returns non-nil
    it becomes anEvent handler: handler.
    - This allows morphs to register themselves as interested.
    - The old handler, if any, is preserved. If the inner handlers throws we
      ensure: into the old handler.
  - self dispatchEvent: anEvent withHandler: handler withMorph: aMorph, since
    the capture/bubbling flow is shared by different types.
  - That tries to call all the submorphs, but if they all #reject (or there are
    none) I try to handle it as well.
    - For each submorph, the dispatcher calls submorph processEvent: anEvent
      and if it doesn't answer #rejected, it was handled. (This recurses way back!)
      - In full Morphic, this does some interesting translation/untranslation.
  - That ends up calling aHandler handleEvent: anEvent in the end.
  - On the way back up, then, each morph in this chain is passed the event
    through #handleEvent:.
    - NB: This mechanism allows morphs that are transparent or otherwise special
      to re-dispatch the event differently to their children.
  - When #handleEvent: is invoked, it double-dispatches the event based on its
    type (anEvent sentTo: self) which calls the appropriate system-level
    handler (eg. #handleClick:)
  - The system-level handler typically checks whether the event was already
    handled, or if this morph is 'disabled' or locked, before calling the
    user-level handler (eg. #click:)"

! Morph
! isLocked
  ^ false

! processEvent: anEvent
  "This is the master entry point for fresh events being processed.
  It's called on the World first, and it (eventually) recursively calls this
  on the frontmost submorph that answers true to #containsEvent:"
  ^ self processEvent: anEvent using: self defaultEventDispatcher

! defaultEventDispatcher
  ^ MorphicEventDispatcher new

! processEvent: anEvent using: aDispatcher
  "Event filtering hooks, if I end up adding that."
  (self rejectsEvent: anEvent) ifTrue: [^#rejected].
  ^ aDispatcher dispatchEvent: anEvent with: self

! rejectsEvent: anEvent
  "Return true to reject this event. Rejecting an event means neither the
  receiver nor any of its submorphs will be given the chance to handle it."
  ^ self isLocked or: [self visible not]

! handleEvent: anEvent
  "This is the immediate handler for this event. Indirect through
  MorphicEvent>>sentTo: so it calls the specific handler, eg. handleClick:"
  anEvent wasIgnored ifFalse: [anEvent sentTo: self].
  ^ anEvent

"System-level event handlers. Users should override eg. #click: and
#keystroke:, not these!"
! handleClick: anEvent
  "System-level click handler. Users should override click:"
  anEvent wasHandled ifTrue: [^self].
  (self handlesClick: anEvent) ifFalse: [^self].
  anEvent wasHandled: true.
  ^ self click: anEvent

! handlesClick: anEvent
  "Do I want to receive click events? False by default, clickable elements
  should return true."
  ^ false
!!


Object subclass: #MorphicEventDispatcher.

! MorphicEventDispatcher
! dispatchEvent: anEvent with: aMorph
  "Split on event type, and call the specific dispatcher."
  Transcript show: 'MorphicEventDispatcher>>dispatchEvent:with:'.
  anEvent isMouse ifTrue: [
    anEvent isMouseButton ifTrue: [^self dispatchClick: anEvent with: aMorph].
    ^ #rejected].
  ^ #rejected

! dispatchClick: anEvent with: aMorph
  "Finds the appropriate receiver for the event, and let it handle it. When
  traveling down the hierarchy a prospective handler for the event is installed.
  This handler can be used by submorphs wishing to handle the click that want
  to negotiate who the true receiver is."
  | handler lastHandler |
  "Quick escapes first."
  Transcript show: 'MorphicEventDispatcher>>dispatchClick:with:'.
  Transcript show: 'fullBounds ', aMorph fullBounds printString, ' position ', anEvent position printString.
  anEvent wasIgnored ifTrue: [^anEvent].
  (aMorph fullBounds containsPoint: anEvent position) ifFalse: [^#rejected].

  lastHandler := anEvent handler.

  handler := aMorph handlerForClick: anEvent.
  handler ifNotNil: [anEvent handler: handler].

  "Call back into the shared recursion logic."
  [^self
    dispatchEvent: anEvent
    withHandler: handler
    withMorph: aMorph
  ] ensure: [
    anEvent handler: lastHandler]

! dispatchEvent: anEvent withHandler: aHandler withMorph: aMorph
  "This is the recursive event dispatcher. We should already have found a
  plausible handler, possibly nil, for this event. We ask submorphs first to
  handle the event, then bubble it up to this morph if not.
  If an event is flagged as ignored, processing stops. ('Ignored' signals it
  was recognized but should do nothing.)

  - The top-most chain of visible, unlocked morphs containing the event position
    each get a chance to handle it.
  - When traveling back up, the prospective handler is always executed. The
    handler needs to check if the event wasHandled, or if someone else's
    handler was been installed.
  - If another handler has been installed but the event was not handled, it
    means that somebody up in the hierarchy wants to handle the event."
  | result filteredEvent |
  Transcript show: 'MorphicEventDispatcher>>dispatchEvent:withHandler:withMorph:'.
  result := self dispatchEvent: anEvent toSubmorphsOf: aMorph.

  Transcript show: 'submorphs result: ', result printString.

  result == #rejected
    ifFalse: [filteredEvent := result]
    ifTrue: [
      "My submorphs did not want it. I might."
      Transcript show: 'children rejected, trying myself'.
      (aMorph containsPoint: anEvent position event: anEvent)
        ifFalse: [^ #rejected].
      filteredEvent := anEvent].

  "Receiver is in the top-most unlocked, visible chain."
  (aHandler notNil and: [filteredEvent wasIgnored not])
    ifTrue: [filteredEvent := aHandler handleEvent: filteredEvent].
  ^ filteredEvent

! dispatchEvent: anEvent toSubmorphsOf: aMorph
  "Dispatch the given event to the submorphs of aMorph.
  TODO Coordinate transforms can be hooked in here, but I don't think we need
  that right now."
  | filteredEvent |
  aMorph submorphsDo: [:child |
    "NB: This recurses to the top-level #processEvent:. Child could use a totally
    different event dispatching scheme from this one."
    filteredEvent := child processEvent: anEvent.
    filteredEvent == #rejected ifFalse: [
      "If we got something else, eg. nil, just keep using the event."
      Transcript show: 'MorphicEventDispatcher>>dispatchEvent:toSubmorphsOf:'.
      filteredEvent isMorphicEvent ifFalse: [filteredEvent := anEvent].
      ^ self nextFromOriginal: anEvent filtered: filteredEvent]].

  ^ #rejected

! nextFromOriginal: originalEvent filtered: filteredEvent
  "This needs overhauling if we allow coordinate transforms."
  ^ filteredEvent == originalEvent
    ifTrue: [originalEvent]
    ifFalse: [
      "There was some transformation. We return a copy and preserve the handler
      state."
      | result |
      result := filteredEvent copy.
      result wasHandled ifFalse: [result copyHandlerState: originalEvent].
      result]

!!



Object subclass: #MorphicEvent
  instanceVariableNames: 'timeStamp source'.
MorphicEvent subclass: #UserInputEvent
  instanceVariableNames: 'type buttons position handler wasHandled wasIgnored'.
UserInputEvent subclass: #MouseEvent.
MouseEvent subclass: #MouseButtonEvent
  instanceVariableNames: 'whichButton nClicks'.

! MorphicEvent
! isMorphicEvent
  ^ true
! isMouse
  ^ false
!!

! UserInputEvent
! initialize
  super initialize.
  wasHandled := wasIgnored := false

! type
  ^ type
! type: aSymbol
  type := aSymbol

! position
  ^ position
! position: anObject
  position := anObject

! wasHandled
  ^ wasHandled
! wasHandled: aBoolean
  wasHandled := aBoolean

! wasIgnored
  ^ wasIgnored
! wasIgnored: aBoolean
  wasIgnored := aBoolean
!!

! MouseEvent
! isMouse
  ^ true
! isMouseButton
  ^ false
!!

! MouseButtonEvent
! isMouseButton
  ^ true
!!

