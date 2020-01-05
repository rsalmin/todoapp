.PHONY: All done

All: todo done

todo: todo.hs CmdArgs.hs PutText.hs Print.hs
	ghc $^

done:
	@echo "***Done***"
