(in-package :motd-admin)

(defconstant +add-tag-url+ "motds/add-tag")

(defun add-tag (message-id tag)
  "Add a new TAG for the message with the given MESSAGE-ID."
  (check-type message-id motd-commands:message-id)
  (check-type tag motd-commands:tag)
  (send-command +add-tag-url+
                (motd-commands:add-tag message-id tag)
                'motd-commands:add-tag-response))
