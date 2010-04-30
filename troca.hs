-- (c) M.P.I 1998/99 a 2005/06 -- Date: 05.10.13

import Mpi  

troca f g h k = [ either (split f g)(split h k),split (either f h)(either g k) ]

{-- teste da lei da troca:
	escrever eg. "testa_lei_da_troca id (==0) succ (==1) (i1 0)".
        Deverá obter uma lista de pares de valores iguais, correspondendo à
	aplicação do lado esquerdo e direito da lei ao argumento (i1 0).
--}

testa_lei_da_troca f g h k x = (fmap ap) (rstr (troca f g h k,x))

