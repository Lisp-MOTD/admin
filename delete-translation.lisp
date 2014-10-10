(in-package :motd-admin)

(defconstant +delete-translation-url+ "motds/delete-translation")

(defun delete-translation (message-id language)
  "Delete an existing translation for the message with the given MESSAGE-ID.  The translation removed is the one with the given LANGUAGE."
  (check-type message-id motd-commands:message-id)
  (check-type language motd-commands:language)
  (send-command +delete-translation-url+
                (motd-commands:delete-translation message-id language)
                'motd-commands:delete-translation-response))
