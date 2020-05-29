# Solucionador de Palavras Cruzadas

O objetivo deste projecto é escrever um programa em PROLOG para resolver puzzles de palavras cruzadas, de agora em diante designados apenas por "puzzles".
Um puzzle de palavras cruzadas é constituído por uma lista de palavras e uma grelha n×m. Cada posição da grelha pode estar vazia, conter uma letra ou estar a negro.

Um puzzle é representado por uma lista de dois elementos:
• O primeiro elemento é uma lista de palavras, • O segundo elemento é uma grelha. Uma grelha de dimensão n×m é representada por uma lista de n listas de m elementos, em que cada uma das n listas representa uma linha do puzzle. Cada elemento é por sua vez:
• uma variável, se a posição correspondente do puzzle não estiver preenchida, • o valor da posição correspondente do puzzle, se esta estiver preenchida, • o símbolo "#", se a posição estiver a negro.
Por exemplo, o puzzle da Fig. 1 é representado por
[[ato,dao,dia,mae,sede,soar,ameno,drama,mande], [[P11, P12, P13, #, P15, P16, P17, P18], [P21, #, P23, P24, P25, #, P27, #], [P31, #, P33, #, P35, a, P37, #], [P41, P42, P43, P44, P45, #, #, #], [P51, #, P53, #, #, #, #, #]]]
