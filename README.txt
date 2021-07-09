Nota:
Al ejecutar las funciones que corren el juego (e.j. runMatch, runGame) es necesario en la tupla de OnitamaAgent 
ingresar en el primer lugar al OnitamaAgent que corresponda al RedPlayer y en el segundo lugar el que corresponde
al BluePlayer. De lo contrario, el programa lanzará una excepción.
Ej: (RandomAgent RedPlayer, RandomAgent BluePlayer) --> Entrada correcta.
    (RandomAgent BluePlayer, RandomAgent RedPlayer) --> Entrada incorrecta.
