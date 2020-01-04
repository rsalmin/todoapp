.PHONY: All done

All: todo done

todo: todo.hs CmdArgs.hs ToText.hs Print.hs
	ghc $^

done:
	@echo "***Done***"
