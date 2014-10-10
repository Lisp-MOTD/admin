(in-package :motd-admin)

(defconstant +propose-message-url+ "motds/propose-message")

(defun propose-message ()
  "Request a new message-id to build into a message."
  (send-command +propose-message-url+
                motd-commands:new-motd
                'motd-commands:new-motd-response))
