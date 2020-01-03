.PHONY: All done

All: todo done

todo: todo.hs CmdArgs.hs ToText.hs
	ghc $^

done:
	@echo "***Done***"
