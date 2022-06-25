(in-package :colitrsynth)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; from audio.lisp
(defclass audio ()
  ((device-api
    :initarg :device-api
    :initform "ASIO"
    ;; :initform "MME"
    :accessor .device-api)
   (device-name
    :initarg :device-name
    :initform "Prism Sound USB Audio Class 2.0"
    ;; :initform "FL Studio ASIO"
    ;; :initform "Realtek Digital Output (Realtek"
    :accessor .device-name)
   (sample-rate
    :initarg :sample-rate
    :initform *sample-rate*
    :type single-float
    :accessor .sample-rate)
   (frames-per-buffer
    :initarg frames-per-buffer
    :initform *frames-per-buffer*
    :accessor .frames-per-buffer)
   (sample-format
    :initarg sample-format
    :initform :float
    :accessor .sample-format)
   (processing :initform nil :accessor .processing)
   (playing :initform nil :accessor .playing)
   (played :initform nil :accessor .played)
   (request-stop :initform nil :accessor .request-stop)
   (stream
    :initform nil
    :accessor .stream)
   (input-channels
    :initarg :input-channels
    :initform 0
    :type fixnum
    :accessor .input-channels)
   (output-channels
    :initarg :output-channels
    :initform 2
    :type fixnum
    :accessor .output-channels)
   (sequencer :initarg :sequencer :accessor .sequencer)
   (master :accessor .master)
   (process-thread :initform nil :accessor .process-thread)
   (process-thread-mailbox :accessor .process-thread-mailbox
                           :initform (sb-concurrency:make-mailbox))
   (audio-mailbox :accessor .audio-mailbox
                  :initform (sb-concurrency:make-mailbox))
   (statistic-enter-time :initform (get-internal-real-time)
                         :accessor .statistic-enter-time)
   (statistic-leave-time :initform (get-internal-real-time)
                         :accessor .statistic-leave-time)
   (statistic-count :initform 0 :accessor .statistic-count)
   (statistic-total-process-time :initform 0
                                 :accessor .statistic-total-process-time)
   (statistic-min-process-time :initform most-positive-fixnum
                               :accessor .statistic-min-process-time)
   (statistic-max-process-time :initform 0
                               :accessor .statistic-max-process-time)
   (statistic-total-interval-time :initform 0
                                  :accessor .statistic-total-interval-time)
   (statistic-min-interval-time :initform most-positive-fixnum
                                :accessor .statistic-min-interval-time)
   (statistic-max-interval-time :initform 0
                                :accessor .statistic-max-interval-time)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; from model.lisp
(defclass name-mixin ()
  ((name :initarg :name :initform "" :accessor .name)))

(defclass connection ()
  ((src :initarg :src :accessor .src)
   (dest :initarg :dest :accessor .dest)))

(defclass audio-connection (connection)
  ((src-bus :initarg :src-bus :initform 0 :accessor .src-bus)
   (dest-bus :initarg :dest-bus :initform 0 :accessor .dest-bus)))

(defclass midi-connection (connection)
  ())

 (defclass param-connection (connection)
  ((param :initarg :param :accessor .param)))

(defclass builtin-param-connection (param-connection)
  ())

(defclass plugin-param-connection (param-connection)
  ())

(defclass model (name-mixin)
  ((in :initarg :in :accessor .in :initform nil)
   (in-count :initform 0 :accessor .in-count :type fixnum)
   (out :initarg :out :accessor .out :initform nil)))

(defclass midi-input-mixin ()
  ((midi-events :initarg :midi-events :initform nil
                :accessor .midi-events)
   (midi-frame :initarg :midi-frame :initform nil
               :accessor .midi-frame)))

(defclass pattern-position ()
  ((pattern :initarg :pattern :accessor .pattern)
   (start :initarg :start :accessor .start)
   (end :initarg :end :accessor .end)
   (last-notes :accessor .last-notes
               :initform (make-array 16 :initial-element off))))

(defclass track (model)
  ((pattern-positions :initform nil :accessor .pattern-positions)
   (buffer :accessor .buffer
           :initform (make-buffer :initial-element nil :element-type t))))

(defclass sequencer (model)
  ((bpm :initarg :bpm :initform 140.0 :accessor .bpm
        :type single-float)
   (lpb :initarg :lpb :initform 4 :accessor .lpb)
   (tracks :initarg :tracks :accessor .tracks :initform nil)
   (end :initform 0 :accessor .end)
   (looping :initform t :accessor .looping)
   (loop-start-line :initform 0 :accessor .loop-start-line
                    :type fixnum)
   (loop-end-line :initform 0 :accessor .loop-end-line
                  :type fixnum)
   (play-position :initform (make-play-position)
                  :accessor .play-position)
   (last-play-position :initform (make-play-position)
                       :accessor .last-play-position)))

(defclass column ()
  ((note :initarg :note :initform none :accessor .note)
   (velocity :initarg :velocity :initform *default-velocity*
             :accessor .velocity :type (unsigned-byte 8))
   (delay :initarg :delay :initform 0 :accessor .delay
          :type (unsigned-byte 8))))

(defclass line ()
  ((columns :initarg :columns :accessor .columns
            :initform (make-array 16 :initial-contents
                                  (loop repeat 16 collect (make-instance 'column))))))

(defclass pattern-mixin (model)
  ((nlines :initarg :nlines :initform #x40 :accessor .nlines)
   (lines :initarg :lines :accessor .lines)
   (current-line :initform 0 :accessor .current-line)))

(defclass pattern (pattern-mixin)
  ((ncolumns :initarg :ncolums :initform 1 :accessor .ncolumns)
   (velocity-enables :initarg :velocity-enables
                     :accessor .velocity-enables
                     :initform (make-array 16 :initial-element nil)
                     :type (simple-array bool (16)))
   (delay-enables :initarg :delay-enables
                  :accessor .delay-enables
                  :initform (make-array 16 :initial-element nil)
                  :type (simple-array bool (16)))))

(defclass lfo (model)
  ((buffer :initform (make-buffer) :accessor .buffer)
   (frequency :initarg :frequency :initform 1.0 :accessor .frequency)
   (phase :initform 0.0 :accessor .phase :type single-float)
   (unipolar-p :initarg :unipolar-p :initform t :accessor .unipolar-p)))

(defclass osc (midi-input-mixin model)
  ((note :initarg :note :initform off :accessor .note)
   (buffer :initform (make-buffer) :accessor .buffer)
   (value :initform 0.0 :accessor .value :type single-float)
   (phase :initform 0.0 :accessor .phase :type single-float)))

(defclass sin-osc (osc)
  ()
  (:default-initargs :name "Sin"))

(defclass saw-osc (osc)
  ()
  (:default-initargs :name "Saw"))

(defclass adsr (midi-input-mixin model)
  ((a :initarg :a :initform 0.003 :accessor .a)
   (d :initarg :d :initform 0.05 :accessor .d)
   (s :initarg :s :initform 0.3 :accessor .s)
   (r :initarg :r :initform 0.1 :accessor .r)
   (buffer :initform (make-buffer) :accessor .buffer)
   (last-gate :initform nil :accessor .last-gate)
   (last-value :initform nil :accessor .last-value)
   (frame :initform 0 :accessor .frame :type fixnum)
   (release-time :initform 0.0 :accessor .release-time)
   (release-value :initform 0.0 :accessor .release-value)))

(defclass operand (model)
  ((left :initarg :left :accessor .left)
   (right :initarg :right :accessor .right)
   (initial-value :initarg :initial-value :accessor .initial-value)))

(defclass op-add (operand)
  ()
  (:default-initargs :left (make-buffer :initial-element 0.0)
                     :right (make-buffer :initial-element 0.0)
                     :initial-value 0.0))

(defclass op-multi (operand)
  ()
  (:default-initargs :left (make-buffer :initial-element 1.0)
                     :right (make-buffer :initial-element 1.0)
                     :initial-value 1.0))

(defclass left-right-buffer-mixin ()
  ((left :initform (make-buffer) :accessor .left)
   (right :initform (make-buffer) :accessor .right)))

(defclass gain (left-right-buffer-mixin model)
  ((volume :initarg :volume :initform 1.0 :accessor .volume)))

(defclass master (left-right-buffer-mixin model)
  ((volume :initform 0.6 :accessor .volume)
   (last-left :initform 0.0 :accessor .last-left)
   (last-right :initform 0.0 :accessor .last-right)))

(defstruct builtin-parameter
  name
  accessor)

(defstruct plugin-parameter
  index
  name
  value
  value-as-text)

(defclass plugin-model (model)
  ((plugin-description :initarg :plugin-description :accessor .plugin-description)
   (host-process :accessor .host-process)
   (host-io :accessor .host-io)
   (out-buffer :accessor .out-buffer)
   (out-length :initform 0 :accessor .out-length)
   (in-buffer :accessor .in-buffer
              :initform (make-array *frames-per-buffer* :element-type 'single-float))
   (left-buffer :accessor .left-buffer)
   (right-buffer :accessor .right-buffer)
   (plugin-state :accessor .plugin-state)
   (params :initform nil :accessor .params)
   (in-params :initform nil :accessor .in-para)
   (latency :initform 0 :accessor .latency)
   (input-nbuses :initform 0 :accessor .input-nbuses)
   (output-nbuses :initform 0 :accessor .output-nbuses)
   (input-nchannels :initform 0 :accessor .input-nchannels)
   (output-nchannels :initform 0 :accessor .output-nchannels)
   (sidechain-nchannels :initform 0 :accessor .sidechain-nchannels)
   (accepts-midi-p :initform nil :accessor accepts-midi-p)
   (produces-midi-p :initform nil :accessor produces-midi-p)
   (midi-effect-p :initform nil :accessor midi-effect-p)
   (mutex :initform (sb-thread:make-mutex) :accessor .mutex)))

(defclass instrument-plugin-model (midi-input-mixin plugin-model)
  ((midi-events :initform nil :accessor .midi-events)
   (output-midi-events :initform nil :accessor .output-midi-events)))

(defclass effect-plugin-model (plugin-model)
  ((mix-buffer :accessor .mix-buffer)))

(defclass plugin-description ()
  ((name :initarg :name :accessor .name)
   (format :initarg :format :accessor .format)
   (category :initarg :category :accessor .category)
   (omanufacturer :initarg :manufacturer :accessor .manufacturer)
   (version :initarg :version :accessor .version)
   (file :initarg :file :accessor .file)
   (unique-id :initarg :unique-id :accessor .unique-id)
   (is-instrument :initarg :is-instrument :accessor .is-instrument)
   (num-inputs :initarg :num-inputs :accessor .num-inputs)
   (num-outputs :initarg :num-outputs :accessor .num-outputs)
   (uid :initarg :uid :accessor .uid)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; from view.lisp

(defconstant +mouse-button-count+ 16)

(defclass app (drag-mixin view)
  ((win :initarg :win :accessor .win)
   (width :initarg :width :initform 800 :accessor .width)
   (height :initarg :height :initform 600 :accessor .height)
   (font :initform nil :accessor .font)
   (views :initarg :views :initform '() :accessor .views)
   (mouse-x :initform 0 :accessor .mouse-x)
   (mouse-y :initform 0 :accessor .mouse-y)
   (selected-modules :initform nil :accessor .selected-modules)
   (selected-pattern :initform nil :accessor .selected-pattern)
   (focused-view :initform nil :accessor .focused-view)
   (click-target-module :initform (make-array +mouse-button-count+))
   (drag-resize-module :initform nil :accessor .drag-resize-module)
   (dragging :initform nil :accessor .dragging)
   (drag-state :initform nil :accessor .drag-state)
   (cable-src :initform nil :accessor .cable-src)
   (song-file :initform nil :accessor .song-file)
   (shift-key-p :initform nil :accessor .shift-key-p)
   (ctrl-key-p :initform nil :accessor .ctrl-key-p)
   (mbox :initform (sb-concurrency:make-mailbox) :accessor .mbox)))

(defclass drag-state ()
  ((target :initarg :target :accessor .target)
   (button :initarg :button :accessor .button)
   (x :initarg :x :accessor .x)
   (y :initarg :y :accessor .y)
   (state :initarg :state :accessor .state)
   (dragging :initform nil :accessor .dragging)))

(defclass function-value-mixin ()
  ((value :initarg :value :initform 0.0)))

(defclass view ()
  ((color :initarg :color :initform *default-color* :accessor .color)
   (x :initarg :x :initform 0 :accessor .x)
   (y :initarg :y :initform 0 :accessor .y)
   (width :initarg :width :initform 100 :accessor .width)
   (height :initarg :height :initform 80 :accessor .height)
   (parent :initarg :parent :initform nil :accessor .parent)
   (children :initarg :children :initform nil :accessor .children)))

(defclass render-border-mixin () ())

(defclass connector (render-border-mixin view)
  ((module :initarg :module :accessor .module))
  (:default-initargs :color *connection-line-color*))

(defclass connector-mixin ()
  ((connector :initarg :connector :accessor .connector)))

(defclass module (drag-resize-mixin
                  drag-move-mixin
                  render-border-mixin
                  view)
  ())

(defclass drag-mixin ()
  ())

(defclass drop-mixin ()
  ())

(defclass drag-move-mixin (drag-mixin)
  ())

(defclass drag-resize-mixin (drag-mixin)
  ())

(defclass label (function-value-mixin view)
  ((last-value :initform "" :accessor .last-value)
   (last-color :accessor .last-color)
   (texture :initform nil :accessor .texture))
  (:default-initargs :width 0 :height 0 :value "くえっ"))

(defclass button (render-border-mixin view)
  ((label-view :accessor .label-view))
  (:default-initargs :width 50 :height 30))

(defclass focus-mixin ()
  ((focused :initform nil :accessor .focused)))

(defclass onchange-mixin ()
  ((onchange :initarg :onchange :initform (constantly nil) :accessor .onchange)))

(defclass text (focus-mixin view)
  ((label :accessor .label)
   (edit-buffer :initform "" :accessor .edit-buffer)
   (cursor-position :initform 0 :accessor .cursor-position)
   (reader :initarg :reader :accessor .reader)
   (writer :initarg :writer :accessor .writer))
  (:default-initargs :height (+ *char-height* 4)
                     :width (+ (* *char-width* 12) 4)))

(defclass slider (onchange-mixin
                  function-value-mixin drag-mixin
                  render-border-mixin view)
  ((min :initarg :min :initform 0.0 :accessor .min)
   (max :initarg :max :initform 1.0 :accessor .max)
   (compute-function :initarg :compute-function
                     :initform #'compute-linear
                     :accessor .compute-function)))

(defclass partial-view (view)
  ((zoom :initarg :zoom :initform 100 :accessor .zoom)
   (offset-x :initarg :offset-x :initform 0 :accessor .offset-x)
   (offset-y :initarg :offset-y :initform 0 :accessor .offset-y)))

(defclass editor-index-label (label)
  ((editor :initarg :editor :accessor .editor))
  (:default-initargs :color *index-color*))

(defclass pattern-editor-note-label (label)
  ((editor :initarg :editor :accessor .editor))
  (:default-initargs :color *note-color*))

(defclass pattern-editor-velocity-label (label)
  ((editor :initarg :editor :accessor .editor))
  (:default-initargs :color *velocity-color*))

(defclass pattern-editor-delay-label (label)
  ((editor :initarg :editor :accessor .editor))
  (:default-initargs :color *delay-color*))

(defclass editor-mixin (partial-view)
  ((model :initarg :model :accessor .model
          ;; 暫定で
          :accessor .pattern)
   (index-labels :initform nil :accessor .index-labels)
   (cursor-x :initform 0 :accessor .cursor-x)
   (cursor-y :initform 0 :accessor .cursor-y)
   (edit-step :initform 0 :accessor .edit-step)
   (keymap :initarg :keymap :accessor .keymap)
   (selection-start :initform nil :accessor .selection-start)
   (selection-end :initform nil :accessor .selection-end)
   (mode :initform :command :accessor .mode
         :type (member :command :insert))
   (selection-mode :initform nil :accessor .selection-mode
                   :type (member nil :line :block))))

(defclass automation-editor (editor-mixin)
  ((value-labels :initform nil :accessor .value-labels))
  (:default-initargs :keymap *pattern-editor-command-keymap*))

(defclass pattern-editor (editor-mixin)
  ((note-labels :initform nil :accessor .note-labels)
   (velocity-labels :initform nil :accessor .velocity-labels)
   (delay-labels :initform nil :accessor .delay-labels)
   (octave :initform 4 :accessor .octave)
   (shifting-p :initform nil :accessor .shifting-p))
  (:default-initargs :keymap *pattern-editor-command-keymap*))

(defclass track-view (track
                      drag-mixin
                      drop-mixin
                      connector-mixin
                      render-border-mixin
                      view)
  ()
  (:default-initargs :width 690 :height *track-height*))

(defclass pattern-position-view (pattern-position
                                 drag-mixin
                                 name-mixin
                                 render-border-mixin
                                 view)
  ((move-delta-x :initform 0 :accessor .move-delta-x)))

(defclass sequencer-timeline-view (drag-mixin view)
  ((labels :initform nil :accessor .labels)
   (sequencer :initarg :sequencer :accessor .sequencer)))

(defclass sequencer-partial-view (partial-view)
  ((timeline :initarg :timeline :accessor .timeline)))

(defclass loop-button (button)
  ((fill-color :accessor .fill-color)
   (sequencer :initarg :sequencer :accessor .sequencer))
  (:default-initargs :label "L"))

(defclass sequencer-module (sequencer module)
  ((partial-view :accessor .partial-view))
  (:default-initargs :x 5 :y 5 :width 700 :height 200))

(defclass pattern-module (pattern module)
  ((pattern-editor :accessor .pattern-editor
                   :initform (make-instance 'pattern-editor)))
  (:default-initargs :name "Pattern" :height 300))

(defclass lfo-module (lfo connector-mixin module)
  ((frequency-slider :initarg :frequency-slide :accessor .frequency-slider))
  (:default-initargs :height 45))

(defclass osc-module-mixin ()
  ()
  (:default-initargs :height 50))

(defclass sin-osc-module (sin-osc osc-module-mixin connector-mixin module)
  ()
  (:default-initargs :name "Sin"))

(defclass saw-osc-module (saw-osc osc-module-mixin connector-mixin module)
  ()
  (:default-initargs :name "Saw"))

(defclass adsr-module (adsr connector-mixin module)
  ()
  (:default-initargs :name "Adsr" :height 100))

(defclass automation-module (pattern-mixin module)
  ((editor :accessor .editor)))

(defclass plugin-module (connector-mixin module)
  ()
  (:default-initargs :height 48))

(defclass instrument-plugin-module (instrument-plugin-model plugin-module)
  ())

(defclass effect-plugin-module (effect-plugin-model plugin-module)
  ())

(defclass op-add-module (op-add connector-mixin module)
  ()
  (:default-initargs :width 55 :height 25))

(defclass op-multi-module (op-multi connector-mixin module)
  ()
  (:default-initargs :width 70 :height 25))

(defclass constant-module (left-right-buffer-mixin connector-mixin model module)
  ((value :initarg :value :initform 0.0 :accessor .value)
   (buffer :initform (make-buffer) :accessor .buffer)
   (value-slider :initarg :value-slide :accessor .value-slider))
  (:default-initargs :height 45))

(defclass volume-controller-mixin ()
  ((volume-slider :initarg :volume-slide :accessor .volume-slider)))

(defclass gain-module (gain volume-controller-mixin connector-mixin module)
  ()
  (:default-initargs :height 45))

(defclass master-module (master volume-controller-mixin connector-mixin module)
  ()
  (:default-initargs  :name "Master" :x 695 :y 515
                      :color (list #xff #xa5 #x00 *transparency*)))

(defclass menu-view (render-border-mixin view)
  ((filter :initform nil :accessor .filter)
   (buttons :initform nil :accessor .buttons)))

(defclass connector-menu-view (menu-view)
  ((available-connections :initarg :available-connections
                          :accessor .available-connections)))

(defclass connector-output-view (menu-view)
  ((cables :initarg :cables :accessor .cables)))

(defclass module-menu-view (menu-view)
  ())

(defclass menu-button (button)
  ((onclick :initarg :onclick :accessor .onclick)))

(defclass menu-builtin-button (button)
  ((class :initarg :class :accessor .class)
   (initargs :initarg :initargs :initform nil :accessor .initargs)))

(defclass menu-plugin-button (button)
  ((plugin-description :initarg :plugin-description
                       :accessor .plugin-description)))

(defclass command-dialog (text)
  ((command :initform "" :accessor .command)
   (targets :initarg :targets :accessor .targets))
  (:default-initargs
   :height (+ *char-height* 4)
   :width (+ (* *char-width* 20) (* *layout-space* 2))
   :reader (constantly "")
   :writer #'identity))

