#https://oremacs.com/2015/02/24/emacs-speed-test/
profile:
		emacs -nw -Q -l ~/my-emacs/profile-dotemacs.el \
		--eval "(setq profile-dotemacs-file (setq load-file-name \"$(abspath configuration.el)\"))" \
		-f profile-dotemacs
