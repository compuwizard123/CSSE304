(define eval-file
	(lambda (path env k)
		(let eval-file ([f (open-input-file path)])
			(let ([obj (read f)])
				(if (not (eof-object? obj))
					(begin
						;(printf "\n\n~s\n" obj)
						(eval-exp (syntax-expand (parse-exp obj)) env k)
						(eval-file f)))))))