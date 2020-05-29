%Nome: Eduardo Miranda Numero: 95569

:- [codigo_comum].

%auxiliares
%predicado em que El nao e elemento da lista
nao_membro(_,[]).
nao_membro(El,[P|R]):-
    El\==P,
    nao_membro(El,R).

%predicado que verifica se X pertence a lista sem o unificar
pertence(X, [P|_]):-X==P,!.
pertence(X, [_|R]):-pertence(X,R).

%----------------------------------------------------------------------
%obtem_letras_palavras(Lst_Pals, Letras), em que Lst_Pals e uma lista 
%de palavras, significa que Letras e a lista ordenada cujos elementos 
%sao listas com as letras de cada palavra de Lst_Pals.
%----------------------------------------------------------------------
obtem_letras_palavras(Lst_Pals,Letras):-
    sort(Lst_Pals,Lst_ord),
    listas_de_letras(Lst_ord,Letras).

listas_de_letras([],[]).
listas_de_letras([P|R],Letras):-
    atom_chars(P,El),
    listas_de_letras(R,Letras2),
    Letras=[El|Letras2].

%----------------------------------------------------------------------
%espaco_fila(Fila, Esp), em que Fila e uma fila (linha ou coluna) de 
%uma grelha,significa que Esp e um espaco de Fila.
%----------------------------------------------------------------------
espaco_fila(Fila,Esp):-
    append([Pref,Esp,Suf],Fila),
    length(Esp,N),N>=3,
    nao_membro('#',Esp),
    (last(Pref,Ult),Ult=='#'
    ;
    Pref==[]),
    (nth1(1,Suf,Pri),Pri=='#'
    ;
    Suf==[]).

%----------------------------------------------------------------------
%espacos_fila(Fila, Espacos), em que Fila e uma fila (linha ou coluna) 
%de uma grelha, significa que Espacos e a lista de todos os espacos de 
%Fila, da esquerda para a direita.
%----------------------------------------------------------------------
espacos_fila(Fila,Espacos):-
    bagof(Esp,espaco_fila(Fila,Esp),Espacos),!.
espacos_fila(_,[]).

%----------------------------------------------------------------------
%espacos_puzzle(Grelha, Espacos), em que Grelha e uma grelha, significa 
%que Espacos e a lista de espacos de Grelha.
%----------------------------------------------------------------------
espacos_puzzle(Grelha,Espacos):-
    %Espacosh e a lista de todos os espacos horizontais
    procura_espacos(Grelha,Espacosh),
    mat_transposta(Grelha,Trans),
    %Espacosv e a lista de todos os espacos verticais
    procura_espacos(Trans,Espacosv),
    append(Espacosh,Espacosv,Espacos).

procura_espacos([],[]).
procura_espacos([P_F|R],Espacos):-
    espacos_fila(P_F,Esp1),
    procura_espacos(R,Esp2),
    append(Esp1,Esp2,Espacos).

%----------------------------------------------------------------------
%espacos_com_posicoes_comuns(Espacos, Esp, Esps_com), em que Espacos
%e uma lista de espacos e Esp e um espaco, significa que Esps_com e a 
%lista de espacos com variaveis em comum com Esp, exceptuando Esp.
%----------------------------------------------------------------------
espacos_com_posicoes_comuns(Espacos,Esp1,Esp_com):-
    ve_esp_com(Espacos,Esp1,Esp_com1),
    %predicado que retira o espaco original da lista com os espacos comuns
    select(Esp1,Esp_com1,Esp_com),!.

ve_esp_com([],_,[]).
ve_esp_com([P_E|R_E],Esp1,Esp_com):-
    %predicado que ve se Esp1 e P_E possuem algum elemento em comum
    membro_esp(Esp1,P_E),!,
    ve_esp_com(R_E,Esp1,Esp_com1),
    Esp_com=[P_E|Esp_com1].
ve_esp_com([_|R_E],Esp1,Esp_com):-
    ve_esp_com(R_E,Esp1,Esp_com).

membro_esp([],[]).
membro_esp(Espaco,[P|_]):-
    pertence(P,Espaco),!.
membro_esp(Espaco,[_|R]):-
    membro_esp(Espaco,R).

%----------------------------------------------------------------------
%palavra_possivel_esp(Pal, Esp, Espacos, Letras), em que Pal e uma 
%lista de letras de uma palavra, Esp e um espaco, Espacos e uma lista 
%de espacos, e Letras e uma lista de listas de letras de palavras, 
%significa que Pal e uma palavra possivel para o espaco Esp.
%----------------------------------------------------------------------
palavra_possivel_esp(Pal, Esp, Espacos, Letras):-
    pertence(Pal,Letras),
    pertence(Esp,Espacos),
    %predicado que se for unificavel Esp=Pal
    unificavel(Pal,Esp),
    espacos_com_posicoes_comuns(Espacos,Esp,Esp_com),
    %verifica se existe pelo menos uma palavra que unifica com cada espaco
    palavra_unificavel_aux(Esp_com,Letras).

unificavel(Pal,Esp):-
    length(Pal,N_P),
    length(Esp,N_E),
    N_P==N_E,
    Esp=Pal.

verifica_unificavel(Pal,Esp):-
    length(Pal,N_P),
    length(Esp,N_E),
    N_P==N_E,
    %usa uma copia de modo a nao unificar o espaco
    copia(Esp,Copia),
    Copia=Pal.

copia(Esp,Copia):-
    maplist(copiael,Esp,Copia).

copiael(El,_):-
    var(El),!.
copiael(El,El).

palavra_unificavel_aux([],_).
palavra_unificavel_aux([P_E|R_E],Letras):-
    %percorre todas as palavras para verificar se alguma unifica com P_E
    percorre_letras(P_E,Letras),
    palavra_unificavel_aux(R_E,Letras).

percorre_letras(P_E,[P_L|_]):-
    %predicado semelhante a unificavel pois tambem verifica se e unificavel
    %no entanto nao unifica 
    verifica_unificavel(P_L,P_E),!.
percorre_letras(P_E,[_|R_L]):-
    percorre_letras(P_E,R_L).


%----------------------------------------------------------------------
%palavras_possiveis_esp(Letras, Espacos, Esp, Pals_Possiveis), em
%que Letras e uma lista de listas de letras de palavras, Espacos e uma 
%lista de espacos, Esp e um espaco, significa que Pals_Possiveis e a 
%lista ordenada de palavras possiveis para o espaco Esp.
%----------------------------------------------------------------------
palavras_possiveis_esp(Letras,Espacos, Esp, Pals_Possiveis):-
    findall(Pal,(member(Pal,Letras),palavra_possivel_esp(Pal,Esp,Espacos,Letras)),Pals_Possiveis).

%----------------------------------------------------------------------
%palavras_possiveis(Letras, Espacos, Pals_Possiveis), em que Letras e
%uma lista de listas de letras de palavras e Espacos e uma lista de 
%espacos, significa que Pals_Possiveis e a lista de palavras possiveis.
%----------------------------------------------------------------------
palavras_possiveis(Letras,Espacos,Pals_possiveis):-
    copia(Espacos,C_espacos),
    %usa uma copia de modo aos Espacos nao diminuirem a cada recursao
    pals_possiveis_aux(Letras,Espacos,Pals_possiveis,C_espacos).

pals_possiveis_aux(_,[],[],_).
pals_possiveis_aux([],_,[],_).
pals_possiveis_aux(Letras,[P_E|R_E],Pals_possiveis,Espacos):-
    palavras_possiveis_esp(Letras,Espacos,P_E,Pals_possiveis_esp),
    pals_possiveis_aux(Letras,R_E,Pals_possiveis1,Espacos),
    Pals_possiveis=[[P_E,Pals_possiveis_esp]|Pals_possiveis1],!.

%----------------------------------------------------------------------
%letras_comuns(Lst_Pals, Letras_comuns), em que Lst_Pals e uma lista de
%listas de letras, significa que Letras_comuns e uma lista de pares 
%(pos, letra), significando que todas as listas de Lst_Pals contem a 
%letra letra na posicao pos.
%----------------------------------------------------------------------
letras_comuns([P_P|R_P],Letras_comuns):-
    Pos=1,
    verifica_letras_comuns(P_P,R_P,Pos,Letras_comuns).

verifica_letras_comuns([],_,_,[]).
verifica_letras_comuns([P_letra|R_letra],R_pal,Pos,Letras_comuns):-
    %verifica se a letra e comum a todas as palavras
    verifica_comum(P_letra,R_pal,Pos),!,
    Pos1 is Pos+1,
    verifica_letras_comuns(R_letra,R_pal,Pos1,Letras_comuns1),
    Letras_comuns=[(Pos,P_letra)|Letras_comuns1].
verifica_letras_comuns([_|R_letra],R_pal,Pos,Letras_comuns):-
    Pos1 is Pos+1,
    verifica_letras_comuns(R_letra,R_pal,Pos1,Letras_comuns).

verifica_comum(_,[],_).
verifica_comum(P_L,[P_P|R_P],Pos):-
    nth1(Pos,P_P,X),
    X==P_L,!,
    verifica_comum(P_L,R_P,Pos).

%----------------------------------------------------------------------
%atribui_comuns(Pals_Possiveis), em que Pals_Possiveis e uma lista de
%palavras possiveis, actualiza esta lista atribuindo a cada espaco as 
%letras comuns a todas as palavras possiveis para esse espaco.
%----------------------------------------------------------------------
atribui_comuns([]).
atribui_comuns([P_Pal_poss|R_Pal_poss]):-
    procura_letras(P_Pal_poss),
    atribui_comuns(R_Pal_poss).

procura_letras(P_pal_poss):-
    nth1(1,P_pal_poss,Esp),
    nth1(2,P_pal_poss,Pals_poss),
    letras_comuns(Pals_poss,Letras_comuns),
    %substitui letras comuns no espaco
    substitui_letras(Esp,Letras_comuns).

substitui_letras(_,[]).
substitui_letras(Esp,[P_par|R_par]):-
    (Pos,Letra)=P_par,
    nth1(Pos,Esp,Letra),
    substitui_letras(Esp,R_par).

%----------------------------------------------------------------------
%retira_impossiveis(Pals_Possiveis, Novas_Pals_Possiveis), em que 
%Pals_Possiveis e uma lista de palavras possiveis, significa que
%Novas_Pals_Possiveis e o resultado de tirar palavras impossiveis de
%Pals_Possiveis.
%----------------------------------------------------------------------
retira_impossiveis([],[]).
retira_impossiveis([P_pal_poss|R_pal_poss],Novas_pals_possiveis):-
    tira_impossiveis(P_pal_poss,Novas_pals_poss),!,
    retira_impossiveis(R_pal_poss,Novas_pals_possiveis1),
    Novas_pals_possiveis=[Novas_pals_poss|Novas_pals_possiveis1].

tira_impossiveis(P_pal_poss,Nova_pals_poss):-
    nth1(1,P_pal_poss,Esp),
    nth1(2,P_pal_poss,Pals_poss),
    %predicado em que Nova_pals_poss1 sao as palavras que unificam com Esp
    rec_imp_aux(Esp,Pals_poss,Nova_pals_poss1),
    Nova_pals_poss=[Esp|[Nova_pals_poss1]].

rec_imp_aux(_,[],[]).
rec_imp_aux(Esp,[P_pal|R_pal],Nova_pals_poss):-
    copia(Esp,C_esp),
    %usa uma copia de modo a nao unificar o espaco
    C_esp=P_pal,!,
    rec_imp_aux(Esp,R_pal,Nova_pals_poss1),!,
    Nova_pals_poss=[P_pal|Nova_pals_poss1].
rec_imp_aux(Esp,[_|R_pal],Nova_pals_poss):-
    rec_imp_aux(Esp,R_pal,Nova_pals_poss).

%----------------------------------------------------------------------
%obtem_unicas(Pals_Possiveis, Unicas), em que Pals_Possiveis e uma
%lista de palavras possiveis, significa que Unicas e a lista de 
%palavras unicas de Pals_Possiveis.
%----------------------------------------------------------------------
obtem_unicas([],[]).
obtem_unicas([P_pal_poss|R_pal_poss],Unicas):-
    %predicado que verifica se a lista de palavras associada ao espaco
    %tem length 1, ou seja, e unica
    verifica_unica(P_pal_poss,Pal_unica),!,
    obtem_unicas(R_pal_poss,Unicas1),
    Unicas=[Pal_unica|Unicas1],!.
obtem_unicas([_|R_pal_poss],Unicas):-
    obtem_unicas(R_pal_poss,Unicas).

verifica_unica(P_pal_poss,Pal):-
    nth1(2,P_pal_poss,Lst_pal),
    length(Lst_pal,N),
    N==1,
    nth1(1,Lst_pal,Pal),!.

%----------------------------------------------------------------------
%retira_unicas(Pals_Possiveis, Novas_Pals_Possiveis), em que 
%Pals_Possiveis e uma lista de palavras possiveis, significa que
%Novas_Pals_Possiveis e o resultado de retirar de Pals_Possiveis as 
%palavras unicas.
%----------------------------------------------------------------------
retira_unicas(Pals_Possiveis,Novas_Pals_Possiveis):-
    obtem_unicas(Pals_Possiveis,Unicas),
    retira_unicas_aux1(Pals_Possiveis,Novas_Pals_Possiveis,Unicas).

retira_unicas_aux1([],[],_).
retira_unicas_aux1([P_pal_poss|R_pal_poss],Novas_Pals_Possiveis,Unicas):-
    %predicado que retira as unicas de um espaco individual
    retira_unicas_aux2(P_pal_poss,Unicas,Res),
    retira_unicas_aux1(R_pal_poss,Novas_Pals_Possiveis1,Unicas),
    Novas_Pals_Possiveis=[Res|Novas_Pals_Possiveis1].

retira_unicas_aux2(P_pal_poss,Unicas,Res):-
    nth1(1,P_pal_poss,Esp),
    nth1(2,P_pal_poss,Pals_possiveis),
    length(Pals_possiveis,N),
    N>1,!,
    %predicado que retira as palavras Unicas de Pals_possiveis
    %colocando o resultado em Lst_pal
    retira_pal(Pals_possiveis,Unicas,Lst_pal),
    Res=[Esp,Lst_pal].
retira_unicas_aux2(P_pal_poss,_,Res):-
    Res=P_pal_poss.

retira_pal([],_,[]).
retira_pal([P_pal|R_pal],Unicas,Lst_pal):-
    member(P_pal,Unicas),!,
    retira_pal(R_pal,Unicas,Lst_pal).
retira_pal([P_pal|R_pal],Unicas,Lst_pal):-
    retira_pal(R_pal,Unicas,Lst_pal1),
    Lst_pal=[P_pal|Lst_pal1].

%----------------------------------------------------------------------
%simplifica(Pals_Possiveis, Novas_Pals_Possiveis), em que 
%Pals_Possiveis e uma lista de palavras possiveis, significa que
%Novas_Pals_Possiveis e o resultado de simplificar Pals_Possiveis. Para 
%simplificar uma lista de palavras possiveis, aplicou-se os predicados 
%atribui_comuns, retira_impossiveis e retira_unicas, por esta ordem, 
%ate nao haver mais alteracoes.
%----------------------------------------------------------------------
simplifica(Pals_Possiveis,Novas_Pals_Possiveis):-
    atribui_comuns(Pals_Possiveis),
    retira_impossiveis(Pals_Possiveis,Novas_Pals_Possiveis1),
    retira_unicas(Novas_Pals_Possiveis1,Novas_Pals_Possiveis),
    %se esta condicao se verificar para o ciclo
    Pals_Possiveis=Novas_Pals_Possiveis,!.
simplifica(Pals_Possiveis,Novas_Pals_Possiveis):-
    atribui_comuns(Pals_Possiveis),
    retira_impossiveis(Pals_Possiveis,Novas_Pals_Possiveis1),
    retira_unicas(Novas_Pals_Possiveis1,Novas_Pals_Possiveis2),
    simplifica(Novas_Pals_Possiveis2,Novas_Pals_Possiveis).

%----------------------------------------------------------------------
%inicializa(Puz, Pals_Possiveis), em que Puz e um puzzle, significa que
%Pals_Possiveis e a lista de palavras possiveis simplificada para Puz.
%----------------------------------------------------------------------
inicializa(Puz,Pals_Possiveis):-
    nth1(1,Puz,Palavras),
    nth1(2,Puz,Grelha),
    obtem_letras_palavras(Palavras,Letras),
    espacos_puzzle(Grelha,Espacos),
    palavras_possiveis(Letras,Espacos,Pals_nao_simp),
    simplifica(Pals_nao_simp,Pals_Possiveis).

%----------------------------------------------------------------------
%escolhe_menos_alternativas(Pals_Possiveis, Escolha), em que
%Pals_Possiveis e uma lista de palavras possiveis, significa que 
%Escolha e o elemento de Pals_Possiveis escolhido.
%----------------------------------------------------------------------
escolhe_menos_alternativas([P_pal_poss|R],Escolha):-
    escolhe_menos_alternativas_aux([P_pal_poss|R],Escolha,P_pal_poss).
escolhe_menos_alternativas_aux([],Escolha,Temp):-
    nth1(2,Temp,Pals),
    length(Pals,N),
    N\==1,
    Escolha=Temp.
escolhe_menos_alternativas_aux([P_pal_poss|R_pal_poss], Escolha,Temp):-
    nth1(2,P_pal_poss,Pals),
    nth1(2,Temp,Temp_pal),
    length(Pals,N),
    length(Temp_pal,N_temp),
    %se N for igual a 1 ou for maior que N_temp escolhe a regra de baixo
    (N==1
        ;
    N>=N_temp),!,
    escolhe_menos_alternativas_aux(R_pal_poss,Escolha,Temp).
escolhe_menos_alternativas_aux([P_pal_poss|R_pal_poss],Escolha,_):-
    escolhe_menos_alternativas_aux(R_pal_poss,Escolha,P_pal_poss).

%----------------------------------------------------------------------
%A chamada experimenta_pal(Escolha, Pals_Possiveis,Novas_Pals_Possiveis), 
%em que Pals_Possiveis e uma lista de palavras possiveis, e Escolha e 
%um dos seus elementos (escolhido pelo predicado anterior), segue os 
%seguintes passos:
%1. Sendo Esp e Lst_Pals o espaco e a lista de palavras de Escolha, 
%respectivamente, escolhe uma palavra de Lst_Pals, Pal.(Utilizando o 
%predicado member). 
%2. Unifica Esp com Pal.
%3. Novas_Pals_Possiveis e o resultado de substituir, em Pals_Possiveis, 
%o elemento Escolha pelo elemento [Esp, [Pal]].
%----------------------------------------------------------------------
experimenta_pal(Escolha, Pals_Possiveis,Novas_Pals_Possiveis):-
    nth1(1,Escolha,Esp),
    nth1(2,Escolha,Pals),
    member(Pal,Pals),
    Esp=Pal,
    %troca Escolha por [Esp, [Pal]]
    select(Escolha,Pals_Possiveis,[Esp,[Pal]],Novas_Pals_Possiveis).

%----------------------------------------------------------------------
%resolve_aux(Pals_Possiveis, Novas_Pals_Possiveis), em que
%Pals_Possiveis e uma lista de palavras possiveis, significa que
%Novas_Pals_Possiveis e o resultado de aplicar os predicados 
%escolhe_menos_alternativas,experimenta_pal e simplifica ate todos os
%espacos possuirem apenas uma palavra possivel para cada um.
%----------------------------------------------------------------------
resolve_aux(Pals_Possiveis, Novas_Pals_Possiveis1):-
    %predicado em que N e o maior numero de palavras associadas a um espaco 
    %em Pals_Possiveis
    maior_length1(Pals_Possiveis,N),
    %se isto se verificar continua a fazer o ciclo
    N\==1,!,
    escolhe_menos_alternativas(Pals_Possiveis,Escolha),
    experimenta_pal(Escolha,Pals_Possiveis,Novas_pals_possiveis),
    simplifica(Novas_pals_possiveis,Novas_Pals_Possiveis),
    resolve_aux(Novas_Pals_Possiveis,Novas_Pals_Possiveis1).
resolve_aux(Pals_Possiveis, Novas_Pals_Possiveis1):-
    escolhe_menos_alternativas(Pals_Possiveis,Escolha),
    experimenta_pal(Escolha,Pals_Possiveis,Novas_pals_possiveis),
    simplifica(Novas_pals_possiveis,Novas_Pals_Possiveis1).

maior_length1(Pals_poss,N):-
    A is 0,
    maior_length(Pals_poss,N,A).

maior_length([],N,A):-
    N=A.
maior_length([P_pal_poss|R_pal_poss],N,A):-
    nth1(2,P_pal_poss,Pals),
    length(Pals,Comp),
    Comp>A,!,
    maior_length(R_pal_poss,N,Comp).
maior_length([_|R],N,A):-
    maior_length(R,N,A).