# learn_fortran
- Edição:
Você pode criar e modificar um programa em algum editor. Como exemplos temos gedit e kate.
Eu recomendo o Atom, que junto com o Dropbox permite organizar muito bem nossos projetos.
Sugiro usar Fortran 90, cuja terminação é .f90. Mas quase tudo que veremos vale para
outras versões (f.95, etc).

- Compilação e execução:
O compilador traduz da linguagem que escrevemos para a que pode ser executada no processador (assembly).
Programas Fortran podem ser compilados com o seguinte comando no terminal (no Linux ou Mac):
gfortran nome.f90
Isso cria o arquivo executável a.out. Para executar este arquivo use
./a.out
Se você quiser criar um executável com um nome específico use
gfortran -o nome nome.f90
Para executar, use
./nome

- Comentários:
O símbolo ! é utilizado para comentários. O que estiver depois de ! é desconsiderado pelo compilador.
Em versões antigas do Fortran, se usava c para comentários. Então tenha cuidado para não começar
linhas com palavras que comecem com c.

- Colunas:
Um comando ou conjunto de comandos pode ocupar até a coluna 133. Eu utilizo
!------------------------------------------------------------------------------------------------------------------------------------
para ter uma referência em relação a isso.

Alguns comandos Linux: 
Para lister o conteúdo de uma pasta use:
ls
Para saber o caminho de onde você está use:
pwd
No meu caso, esse comando retornou, por exemplo,
/home/jonasmaziero/Dropbox/GitHub/learn_fortran
Se quiser voltar uma pasta usa:
cd ..
Se quiser voltar para /home (ou /Users) usa:
cd
Para criar uma pasta usar:
mkdir nome
Para deletar um arquivo digita:
rm nome
Para deletar uma pasta usa:
rm -Rf nome
