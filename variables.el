(defcustom --todo-file nil
  "The path to the file with things to do on this machine."
  :type 'file
  :group 'personal)

(defcustom --done-file nil
  "The path to the file with done things on this machine."
  :type 'file
  :group 'personal)

(defcustom --diary-file nil
  "The path to the file with the record of work on this machine."
  :type 'file
  :group 'personal)

(defcustom --notes-folder nil
  "The path to folder where notes are stored on this machine"
  :type 'file
  :group 'personal)

(provide 'config::variables)
