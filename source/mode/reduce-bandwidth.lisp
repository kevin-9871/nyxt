;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(nyxt:define-package :nyxt/mode/reduce-bandwidth
  (:documentation "Package for `reduce-bandwidth-mode' to reduce bandwidth usage when browsing."))
(in-package :nyxt/mode/reduce-bandwidth)

(define-mode reduce-bandwidth-mode (nyxt/mode/no-image:no-image-mode
                                    nyxt/mode/no-script:no-script-mode
                                    nyxt/mode/no-webgl:no-webgl-mode)
  "Reduce bandwidth by enabling `nyxt/mode/no-image:no-image-mode',
`nyxt/mode/no-script:no-script-mode', and `nyxt/mode/no-webgl:no-webgl-mode'.")
