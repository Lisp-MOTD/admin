(in-package :motd-admin)

(defconstant +delete-tag-url+ "motds/delete-tag")

(defun delete-tag (message-id tag)
  "Delete a TAG for the message with the given MESSAGE-ID."
  (check-type message-id motd-commands:message-id)
  (check-type tag motd-commands:tag)
  (send-command +delete-tag-url+
                (motd-commands:delete-tag message-id tag)
                'motd-commands:delete-tag-response))
