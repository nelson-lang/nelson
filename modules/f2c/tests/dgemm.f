*> \brief \b DGEMM
    2 *
    3 *  =========== DOCUMENTATION ===========
    4 *
    5 * Online html documentation available at
    6 *            http://www.netlib.org/lapack/explore-html/
    7 *
    8 *  Definition:
    9 *  ===========
   10 *
   11 *       SUBROUTINE DGEMM(TRANSA,TRANSB,M,N,K,ALPHA,A,LDA,B,LDB,BETA,C,LDC)
   12 *
   13 *       .. Scalar Arguments ..
   14 *       DOUBLE PRECISION ALPHA,BETA
   15 *       INTEGER K,LDA,LDB,LDC,M,N
   16 *       CHARACTER TRANSA,TRANSB
   17 *       ..
   18 *       .. Array Arguments ..
   19 *       DOUBLE PRECISION A(LDA,*),B(LDB,*),C(LDC,*)
   20 *       ..
   21 *
   22 *
   23 *> \par Purpose:
   24 *  =============
   25 *>
   26 *> \verbatim
   27 *>
   28 *> DGEMM  performs one of the matrix-matrix operations
   29 *>
   30 *>    C := alpha*op( A )*op( B ) + beta*C,
   31 *>
   32 *> where  op( X ) is one of
   33 *>
   34 *>    op( X ) = X   or   op( X ) = X**T,
   35 *>
   36 *> alpha and beta are scalars, and A, B and C are matrices, with op( A )
   37 *> an m by k matrix,  op( B )  a  k by n matrix and  C an m by n matrix.
   38 *> \endverbatim
   39 *
   40 *  Arguments:
   41 *  ==========
   42 *
   43 *> \param[in] TRANSA
   44 *> \verbatim
   45 *>          TRANSA is CHARACTER*1
   46 *>           On entry, TRANSA specifies the form of op( A ) to be used in
   47 *>           the matrix multiplication as follows:
   48 *>
   49 *>              TRANSA = 'N' or 'n',  op( A ) = A.
   50 *>
   51 *>              TRANSA = 'T' or 't',  op( A ) = A**T.
   52 *>
   53 *>              TRANSA = 'C' or 'c',  op( A ) = A**T.
   54 *> \endverbatim
   55 *>
   56 *> \param[in] TRANSB
   57 *> \verbatim
   58 *>          TRANSB is CHARACTER*1
   59 *>           On entry, TRANSB specifies the form of op( B ) to be used in
   60 *>           the matrix multiplication as follows:
   61 *>
   62 *>              TRANSB = 'N' or 'n',  op( B ) = B.
   63 *>
   64 *>              TRANSB = 'T' or 't',  op( B ) = B**T.
   65 *>
   66 *>              TRANSB = 'C' or 'c',  op( B ) = B**T.
   67 *> \endverbatim
   68 *>
   69 *> \param[in] M
   70 *> \verbatim
   71 *>          M is INTEGER
   72 *>           On entry,  M  specifies  the number  of rows  of the  matrix
   73 *>           op( A )  and of the  matrix  C.  M  must  be at least  zero.
   74 *> \endverbatim
   75 *>
   76 *> \param[in] N
   77 *> \verbatim
   78 *>          N is INTEGER
   79 *>           On entry,  N  specifies the number  of columns of the matrix
   80 *>           op( B ) and the number of columns of the matrix C. N must be
   81 *>           at least zero.
   82 *> \endverbatim
   83 *>
   84 *> \param[in] K
   85 *> \verbatim
   86 *>          K is INTEGER
   87 *>           On entry,  K  specifies  the number of columns of the matrix
   88 *>           op( A ) and the number of rows of the matrix op( B ). K must
   89 *>           be at least  zero.
   90 *> \endverbatim
   91 *>
   92 *> \param[in] ALPHA
   93 *> \verbatim
   94 *>          ALPHA is DOUBLE PRECISION.
   95 *>           On entry, ALPHA specifies the scalar alpha.
   96 *> \endverbatim
   97 *>
   98 *> \param[in] A
   99 *> \verbatim
  100 *>          A is DOUBLE PRECISION array, dimension ( LDA, ka ), where ka is
  101 *>           k  when  TRANSA = 'N' or 'n',  and is  m  otherwise.
  102 *>           Before entry with  TRANSA = 'N' or 'n',  the leading  m by k
  103 *>           part of the array  A  must contain the matrix  A,  otherwise
  104 *>           the leading  k by m  part of the array  A  must contain  the
  105 *>           matrix A.
  106 *> \endverbatim
  107 *>
  108 *> \param[in] LDA
  109 *> \verbatim
  110 *>          LDA is INTEGER
  111 *>           On entry, LDA specifies the first dimension of A as declared
  112 *>           in the calling (sub) program. When  TRANSA = 'N' or 'n' then
  113 *>           LDA must be at least  max( 1, m ), otherwise  LDA must be at
  114 *>           least  max( 1, k ).
  115 *> \endverbatim
  116 *>
  117 *> \param[in] B
  118 *> \verbatim
  119 *>          B is DOUBLE PRECISION array, dimension ( LDB, kb ), where kb is
  120 *>           n  when  TRANSB = 'N' or 'n',  and is  k  otherwise.
  121 *>           Before entry with  TRANSB = 'N' or 'n',  the leading  k by n
  122 *>           part of the array  B  must contain the matrix  B,  otherwise
  123 *>           the leading  n by k  part of the array  B  must contain  the
  124 *>           matrix B.
  125 *> \endverbatim
  126 *>
  127 *> \param[in] LDB
  128 *> \verbatim
  129 *>          LDB is INTEGER
  130 *>           On entry, LDB specifies the first dimension of B as declared
  131 *>           in the calling (sub) program. When  TRANSB = 'N' or 'n' then
  132 *>           LDB must be at least  max( 1, k ), otherwise  LDB must be at
  133 *>           least  max( 1, n ).
  134 *> \endverbatim
  135 *>
  136 *> \param[in] BETA
  137 *> \verbatim
  138 *>          BETA is DOUBLE PRECISION.
  139 *>           On entry,  BETA  specifies the scalar  beta.  When  BETA  is
  140 *>           supplied as zero then C need not be set on input.
  141 *> \endverbatim
  142 *>
  143 *> \param[in,out] C
  144 *> \verbatim
  145 *>          C is DOUBLE PRECISION array, dimension ( LDC, N )
  146 *>           Before entry, the leading  m by n  part of the array  C must
  147 *>           contain the matrix  C,  except when  beta  is zero, in which
  148 *>           case C need not be set on entry.
  149 *>           On exit, the array  C  is overwritten by the  m by n  matrix
  150 *>           ( alpha*op( A )*op( B ) + beta*C ).
  151 *> \endverbatim
  152 *>
  153 *> \param[in] LDC
  154 *> \verbatim
  155 *>          LDC is INTEGER
  156 *>           On entry, LDC specifies the first dimension of C as declared
  157 *>           in  the  calling  (sub)  program.   LDC  must  be  at  least
  158 *>           max( 1, m ).
  159 *> \endverbatim
  160 *
  161 *  Authors:
  162 *  ========
  163 *
  164 *> \author Univ. of Tennessee
  165 *> \author Univ. of California Berkeley
  166 *> \author Univ. of Colorado Denver
  167 *> \author NAG Ltd.
  168 *
  169 *> \date December 2016
  170 *
  171 *> \ingroup double_blas_level3
  172 *
  173 *> \par Further Details:
  174 *  =====================
  175 *>
  176 *> \verbatim
  177 *>
  178 *>  Level 3 Blas routine.
  179 *>
  180 *>  -- Written on 8-February-1989.
  181 *>     Jack Dongarra, Argonne National Laboratory.
  182 *>     Iain Duff, AERE Harwell.
  183 *>     Jeremy Du Croz, Numerical Algorithms Group Ltd.
  184 *>     Sven Hammarling, Numerical Algorithms Group Ltd.
  185 *> \endverbatim
  186 *>
  187 *  =====================================================================
  188       SUBROUTINE dgemm(TRANSA,TRANSB,M,N,K,ALPHA,A,LDA,B,LDB,BETA,C,LDC)
  189 *
  190 *  -- Reference BLAS level3 routine (version 3.7.0) --
  191 *  -- Reference BLAS is a software package provided by Univ. of Tennessee,    --
  192 *  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
  193 *     December 2016
  194 *
  195 *     .. Scalar Arguments ..
  196       DOUBLE PRECISION ALPHA,BETA
  197       INTEGER K,LDA,LDB,LDC,M,N
  198       CHARACTER TRANSA,TRANSB
  199 *     ..
  200 *     .. Array Arguments ..
  201       DOUBLE PRECISION A(lda,*),B(ldb,*),C(ldc,*)
  202 *     ..
  203 *
  204 *  =====================================================================
  205 *
  206 *     .. External Functions ..
  207       LOGICAL LSAME
  208       EXTERNAL lsame
  209 *     ..
  210 *     .. External Subroutines ..
  211       EXTERNAL xerbla
  212 *     ..
  213 *     .. Intrinsic Functions ..
  214       INTRINSIC max
  215 *     ..
  216 *     .. Local Scalars ..
  217       DOUBLE PRECISION TEMP
  218       INTEGER I,INFO,J,L,NCOLA,NROWA,NROWB
  219       LOGICAL NOTA,NOTB
  220 *     ..
  221 *     .. Parameters ..
  222       DOUBLE PRECISION ONE,ZERO
  223       parameter(one=1.0d+0,zero=0.0d+0)
  224 *     ..
  225 *
  226 *     Set  NOTA  and  NOTB  as  true if  A  and  B  respectively are not
  227 *     transposed and set  NROWA, NCOLA and  NROWB  as the number of rows
  228 *     and  columns of  A  and the  number of  rows  of  B  respectively.
  229 *
  230       nota = lsame(transa,'N')
  231       notb = lsame(transb,'N')
  232       IF (nota) THEN
  233           nrowa = m
  234           ncola = k
  235       ELSE
  236           nrowa = k
  237           ncola = m
  238       END IF
  239       IF (notb) THEN
  240           nrowb = k
  241       ELSE
  242           nrowb = n
  243       END IF
  244 *
  245 *     Test the input parameters.
  246 *
  247       info = 0
  248       IF ((.NOT.nota) .AND. (.NOT.lsame(transa,'C')) .AND.
  249      +    (.NOT.lsame(transa,'T'))) THEN
  250           info = 1
  251       ELSE IF ((.NOT.notb) .AND. (.NOT.lsame(transb,'C')) .AND.
  252      +         (.NOT.lsame(transb,'T'))) THEN
  253           info = 2
  254       ELSE IF (m.LT.0) THEN
  255           info = 3
  256       ELSE IF (n.LT.0) THEN
  257           info = 4
  258       ELSE IF (k.LT.0) THEN
  259           info = 5
  260       ELSE IF (lda.LT.max(1,nrowa)) THEN
  261           info = 8
  262       ELSE IF (ldb.LT.max(1,nrowb)) THEN
  263           info = 10
  264       ELSE IF (ldc.LT.max(1,m)) THEN
  265           info = 13
  266       END IF
  267       IF (info.NE.0) THEN
  268           CALL xerbla('DGEMM ',info)
  269           RETURN
  270       END IF
  271 *
  272 *     Quick return if possible.
  273 *
  274       IF ((m.EQ.0) .OR. (n.EQ.0) .OR.
  275      +    (((alpha.EQ.zero).OR. (k.EQ.0)).AND. (beta.EQ.one))) RETURN
  276 *
  277 *     And if  alpha.eq.zero.
  278 *
  279       IF (alpha.EQ.zero) THEN
  280           IF (beta.EQ.zero) THEN
  281               DO 20 j = 1,n
  282                   DO 10 i = 1,m
  283                       c(i,j) = zero
  284    10             CONTINUE
  285    20         CONTINUE
  286           ELSE
  287               DO 40 j = 1,n
  288                   DO 30 i = 1,m
  289                       c(i,j) = beta*c(i,j)
  290    30             CONTINUE
  291    40         CONTINUE
  292           END IF
  293           RETURN
  294       END IF
  295 *
  296 *     Start the operations.
  297 *
  298       IF (notb) THEN
  299           IF (nota) THEN
  300 *
  301 *           Form  C := alpha*A*B + beta*C.
  302 *
  303               DO 90 j = 1,n
  304                   IF (beta.EQ.zero) THEN
  305                       DO 50 i = 1,m
  306                           c(i,j) = zero
  307    50                 CONTINUE
  308                   ELSE IF (beta.NE.one) THEN
  309                       DO 60 i = 1,m
  310                           c(i,j) = beta*c(i,j)
  311    60                 CONTINUE
  312                   END IF
  313                   DO 80 l = 1,k
  314                       temp = alpha*b(l,j)
  315                       DO 70 i = 1,m
  316                           c(i,j) = c(i,j) + temp*a(i,l)
  317    70                 CONTINUE
  318    80             CONTINUE
  319    90         CONTINUE
  320           ELSE
  321 *
  322 *           Form  C := alpha*A**T*B + beta*C
  323 *
  324               DO 120 j = 1,n
  325                   DO 110 i = 1,m
  326                       temp = zero
  327                       DO 100 l = 1,k
  328                           temp = temp + a(l,i)*b(l,j)
  329   100                 CONTINUE
  330                       IF (beta.EQ.zero) THEN
  331                           c(i,j) = alpha*temp
  332                       ELSE
  333                           c(i,j) = alpha*temp + beta*c(i,j)
  334                       END IF
  335   110             CONTINUE
  336   120         CONTINUE
  337           END IF
  338       ELSE
  339           IF (nota) THEN
  340 *
  341 *           Form  C := alpha*A*B**T + beta*C
  342 *
  343               DO 170 j = 1,n
  344                   IF (beta.EQ.zero) THEN
  345                       DO 130 i = 1,m
  346                           c(i,j) = zero
  347   130                 CONTINUE
  348                   ELSE IF (beta.NE.one) THEN
  349                       DO 140 i = 1,m
  350                           c(i,j) = beta*c(i,j)
  351   140                 CONTINUE
  352                   END IF
  353                   DO 160 l = 1,k
  354                       temp = alpha*b(j,l)
  355                       DO 150 i = 1,m
  356                           c(i,j) = c(i,j) + temp*a(i,l)
  357   150                 CONTINUE
  358   160             CONTINUE
  359   170         CONTINUE
  360           ELSE
  361 *
  362 *           Form  C := alpha*A**T*B**T + beta*C
  363 *
  364               DO 200 j = 1,n
  365                   DO 190 i = 1,m
  366                       temp = zero
  367                       DO 180 l = 1,k
  368                           temp = temp + a(l,i)*b(j,l)
  369   180                 CONTINUE
  370                       IF (beta.EQ.zero) THEN
  371                           c(i,j) = alpha*temp
  372                       ELSE
  373                           c(i,j) = alpha*temp + beta*c(i,j)
  374                       END IF
  375   190             CONTINUE
  376   200         CONTINUE
  377           END IF
  378       END IF
  379 *
  380       RETURN
  381 *
  382 *     End of DGEMM .
  383 *
  384       END
