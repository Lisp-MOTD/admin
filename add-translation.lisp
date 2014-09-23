(in-package :motd-admin)

(defconstant +add-translation-url+ "motds/add-translation")

(defun add-translation (message-id language text)
  "Add a new translation (or replace a given translation) for the message with the given MESSAGE-ID.  The new translation has the given LANGUAGE and TEXT."
  (check-type message-id motd-commands:message-id)
  (check-type language motd-commands:language)
  (check-type text motd-commands:text)
  (send-command +add-translation-url+
                (motd-commands:add-translation message-id language text)
                'motd-commands:add-translation-response))
