%
%
%
%

%(SepalLength, SepalWidth, PetalLength, PetalWidth)



% setosa rules
setosa_rule(_,_,_,PetalWidth) :- PetalWidth =< 0.8.


% rules of versicolor
versicolor_classification(SepalLength, SepalWidth, PetalLength, PetalWidth) :- 
    (
        % First rule leading to versicolor
        PetalWidth > 0.8,
        PetalWidth =< 1.75,
        PetalLength =< 4.95,
        PetalWidth =< 1.65
    ) 
    ; 
    (
        % Second rule leading to versicolor
        PetalWidth > 0.8,
        PetalWidth =< 1.75,
        PetalLength > 4.95,
        PetalWidth > 1.55,
        SepalLength =< 5.95
    ) 
    ; 
    (
        % Third rule leading to versicolor
        PetalWidth > 1.75,
        PetalLength > 4.85,
        SepalWidth > 3.1
    ).


virginica_classification(SepalLength, SepalWidth, PetalLength, PetalWidth) :- 
    (
        % First rule leading to virginica
        PetalWidth > 1.75,
    ) 
    ; 
    (
        % Second rule leading to virginica
        PetalWidth =< 1.75,
        PetalLength > 4.95,
        PetalWidth > 1.55
    ) 
    ; 
    (
        % Third rule leading to virginica
        PetalWidth =< 1.75,
        PetalLength > 4.95,
        PetalWidth =< 1.55,
        SepalLength > 5.95
    ) 
    ; 
    (
        % Fourth rule leading to virginica
        PetalWidth =< 1.75,
        PetalLength > 4.85,
        SepalWidth =< 3.1
    ).




classify(SepalLength, SepalWidth, PetalLength, PetalWidth,Class) :-
    (   % Setosa classification rule.
        setosa_rule(SepalLength, SepalWidth, PetalLength, PetalWidth) -> Class = 'Iris-setosa'
    ;   % Versicolor classification rules.
        (   versicolor_classification(SepalLength, SepalWidth, PetalLength, PetalWidth) ) -> Class = 'Iris-versicolor'
    ;   % Virginica classification rules.
        (   virginica_classification(SepalLength, SepalWidth, PetalLength, PetalWidth)) -> Class = 'Iris-virginica'
    ;   % Default classification if none of the above rules apply.
        Class = 'Unknown'
    ).
