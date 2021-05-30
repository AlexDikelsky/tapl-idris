#lang clotho
(require xsmith
         racr
         racket/string)
 
(define-spec-component arith)
 
(add-to-grammar
 arith
 [Program #f (Expression)]
 [Expression #f ()
             #:prop may-be-generated #f]
 [LiteralInt Expression ([v = 0])]
 [Succ Expression ([r : Expression])]
 [Pred Expression ([r : Expression])]
 [IsZero Expression ([r : Expression])]
 [If Expression ([test : Expression] 
		 [trueclause : Expression]
		 [falseclause : Expression])]
 )
 
(define int (base-type 'int))
(add-property
 arith
 type-info
 [Program [int (λ (n t) (hash 'Expression int))]]
 [LiteralInt [int (λ (n t) (hash))]]
 [Succ   [int (λ (n t) (hash 'r int))]]
 [Pred   [int (λ (n t) (hash 'r int))]]
 [IsZero [int (λ (n t) (hash 'r int))]]
 [If     [int (λ (n t) (hash 'test int 'trueclause int 'falseclause int))]]
 )
 
(add-property
 arith
 render-node-info
 [Program
  (λ (n) (att-value 'xsmith_render-node (ast-child 'Expression n)))]
 [LiteralInt
  (λ (n) (number->string (ast-child 'v n)))]
 [Succ
  (λ (n) (format "(succ ~a)"
                 (att-value 'xsmith_render-node (ast-child 'r n))))]
 [Pred
  (λ (n) (format "(pred ~a)"
                 (att-value 'xsmith_render-node (ast-child 'r n))))]
 [IsZero
  (λ (n) (format "(iszero ~a)"
                 (att-value 'xsmith_render-node (ast-child 'r n))))]
 [If
  (λ (n) (format "(if ~a then ~a else ~a)"
        	 (att-value 'xsmith_render-node (ast-child 'test n))
                 (att-value 'xsmith_render-node (ast-child 'trueclause n))
                 (att-value 'xsmith_render-node (ast-child 'falseclause n))))]
 )
 
;; This line defines `arith-command-line`.
(define-xsmith-interface-functions
  [arith]
  #:comment-wrap (λ (lines)
                   (string-join
                    (map (λ (x) (format "// ~a" x)) lines)
                    "\n")))
 
(module+ main (arith-command-line))
