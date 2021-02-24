
    
******* PARAMETERS FOR THE SUSPENSE ALGORITHM *********
    
attentiondecay(0.88).  % the rate at which threads decay in importance when they are not explicitly evoked in a story step

highnumberofsteps(7).  % the cut-off default maximum number of steps in which it is estimated that a thread will be interrupted or disallowed, unless there is an active disallowing event already. 

interruptcompletionratio(0.7).  % the ratio of the effect of completion and interrupt imminence ie FUTURE events for suspense calculation, equality would be represented by 0.5, a higher number boosts the importance of completion with respect to imminence. 

conflictingandconfirmedratio(1.5). % the importance of conflicting PAST events on the confidence level of a thread, a higher number means that the importance of conflicting events is greater relative to the number of confirmed events in a thread, that is events that have actually been explicitly told

confidencethreshold(0.95). % the threshold of confidence above which a thread 'has the right' to confidencethreshold another one. 

thresholdfordisplay(0).
    
**************


start:- 
    getstory(E), 
    tell(E,[],[0],S), % takes a story and returns a list of suspense levels for each step in the story
    
    writefinallist([start|E],S),
    writelatexlist(-1,S).

tell([],_,S,S). 
    % when we have told all the story, 
    % we return the final set of suspense values

tell([Event|Untold],Arcs,SValues,FinalSValues):-

    maintenance(Event,Arcs,Newarcs), 
        % adding and subtracting arcs according to the new event
    
    calcsuspensefromarcs(Newarcs,Newarcs,NewSuspenseList), 
        % calculating the current suspense level first from each separate arc
    
    calcglobalsuspense([0|NewSuspenseList],GlobalSuspense), 
    
    write('-----------------------------------'),nl,nl,

    write('Event is : '),write(Event),nl,nl,
    write('Suspense at this step is '),format( '~2f', [GlobalSuspense]),nl,nl,
    
    
    writeoutimportantthreads(Newarcs,NewSuspenseList),nl,nl,
    
    
    % nl,writedoublelist(Newarcs,NewSuspenseList),nl,
    
    
    write('Event is : '),write(Event),nl,nl,
    write('Suspense at this step is '),format( '~2f', [GlobalSuspense]),nl,nl,
    
    write('-----------------------------------'),nl,nl,
    
        % then using an evaluation of the spread in suspense values 
        % to calculate the global suspense for the narrative at this point
        % if all values are negative or positive, then we use zero as one value to obtain the spread
    
    addtotail(GlobalSuspense,SValues,NewSValues),
    
    tell(Untold,Newarcs,NewSValues,FinalSValues). 
        % telling the next event
        % design choice : we don't include old events, nor old arcs ---  headlong approach
    
    
    
    %*************SUSPENSE FUNCTIONS***************************
    
    
        calcsuspensefromarcs([],_,[]).
        calcsuspensefromarcs([Arc|Arcs],TotalArcs,[Susp|Susps]):- 
            % go through all arcs and calculate suspense from them
            calcsuspensefromarc(Arc,TotalArcs,Susp),
            calcsuspensefromarcs(Arcs,TotalArcs,Susps).
        
                    
        calcsuspensefromarc(Arc,Arcs,Susp):-
            findstepstocompletion(Arc,Arcs, Stepstocompletion),
            findstepstointerrupt(Arc,Arcs,Arcs,Stepstointerruptlist),
            min_list(Stepstointerruptlist,Mininterruptsteps),
            % put suspense from arc due to potential increase in confidence level here ! 
        
        suspensealgorithm(Arc,Stepstocompletion,Mininterruptsteps,Susp).

                % ----------------------------------%
                
         findstepstocompletion(arc(OE1,_Val1,_Att1,
                _Confirmed1,_Confidence1,Untold1,[]),Currentarcs,Stepstocompletion):-   
                member(Ev,Untold1), 
                member(arc(OE2,_Val2,_Att2,_Confirmed2,_Confidence2,Untold2,Told2),Currentarcs),
                member(_Z,Told2), % Told2 <> [] : check that the  told arcs are nonempty first
                \+ OE1 == OE2,
                member(Ev,Untold2),
                % find place of Ev, and add to size of first arc
                eventsbefore(Ev,Untold2,Place1),
                eventstoclosure(Ev,Untold1,Place2),
                Stepstocompletion is Place1 + Place2.
                
         findstepstocompletion(arc(OE1,_Val1,_Att1,_Confirmed1,_Confidence1,Untold1,[]),Currentarcs,
                         Stepstocompletion):-   
                member(Ev,Untold1), 
                member(arc(OE2,Val2,Att2,Confirmed2,Confidence2,Untold2,[]),Currentarcs),
                \+ OE1 == OE2,
                member(Ev,Untold2),             
                takeout(arc(_OE1,_Val1,_Att1,_Confirmed1,_Confidence1,Untold1,[]),Currentarcs,NewCurrentarcs),
                findstepstocompletion(arc(OE2,Val2,Att2,Confirmed2,Confidence2,Untold2,[]),
                            NewCurrentarcs,Place1), % we know that this is a non confirmed arc, told is empty
                eventstoclosure(Ev,Untold1,Place2),
                Stepstocompletion is Place1 + Place2.
        findstepstocompletion(arc(_OE1,_Val1,_Att1,_Confirmed1,_Confidence1,Untold1,_Told),_Currentarcs,
                         Stepstocompletion):-
                size(Untold1,Stepstocompletion).
                
        takeout(_A,[],[]).
        takeout(H,[H|T],T).
        takeout(A,[H|T1],[H|T2]):-
            takeout(A,T1,T2).
            
        
        
            
        eventstoclosure(Ev,[Ev|T],Place1):- 
                size(T,Place1).
        eventstoclosure(Ev,[_H|T],Place):- 
                eventstoclosure(Ev,T,Place). 
        
        eventsbefore(Ev,[Ev|_T],1).
        eventsbefore(Ev,[_H|T],Place1):-
                eventsbefore(Ev,T,Place2),
                Place1 is Place2 + 1.
                
                
        findstepstointerrupt(_,[],_,[]).
        findstepstointerrupt(Arc,[Arc|Arcs],CurrentArcs,[Stepstointerrupt|Steps]):-
                highnumberofsteps(Stepstointerrupt),
                findstepstointerrupt(Arc,Arcs,CurrentArcs,Steps). % don't check if same arc
        findstepstointerrupt(Arc,[Arc1|Arcs],CurrentArcs,[Stepstointerrupt|Steps]):-
                findstepstointerruptfromonearc(Arc,Arc1,CurrentArcs,Stepstointerrupt),
                findstepstointerrupt(Arc,Arcs,CurrentArcs,Steps).
                
                % find arcs it interrupts and then multiply suspense effect by value and attention of these
                 
                
                
            
        findstepstointerruptfromonearc(arc(_OE,_Val,_Att,_Confirmed,_Confidence,Untold,_Told),
                arc(_OE1,_Val1,_Att1,_Confirmed1,_Confidence1,Untold1,Told1),_CurrentArcs,Stepstointerrupt):-
                member(_Z,Told1), % filtering the confirmed arcs
                member(Event,Untold),
                disallow(Event1,Event),
                member(Event1,Untold1),
                stepstoevent(Untold1,Event1,Stepstointerrupt).
                
        findstepstointerruptfromonearc(arc(_OE,_Val,_Att,_Confirmed,_Confidence,Untold,_Told),
                arc(OE1,Val1,Att1,Confirmed1,Confidence1,Untold1,[]),CurrentArcs,Stepstointerrupt):-
                % we know that OE1 is unconfirmed and Told1 is empty
                member(Event,Untold),
                disallow(Event1,Event),
                member(Event1,Untold1),
                findstepstoconfirminterruptingarc(arc(OE1,Val1,Att1,Confirmed1,Confidence1,Untold1,[]),CurrentArcs,Event1,Stepstointerrupt).
        
        findstepstointerruptfromonearc(_Arc,_Arc1,_CurrentArcs,Stepsinfinite):-
                highnumberofsteps(Stepsinfinite).
                
        findstepstoconfirminterruptingarc(arc(OE1,_Val1,_Att1,_Confirmed1,_Confidence1,Untold1,[]),CurrentArcs,Event1,Stepstointerrupt):-
                member(Ev,Untold1), 
                member(arc(OE2,_Val2,_Att2,_Confirmed2,_Confidence2,Untold2,_Told2),CurrentArcs),
                % member(_Z,Told2), % Told2 <> [] : check that the  told arcs are nonempty first
                \+ OE1 == OE2,
                member(Ev,Untold2),
                eventsbefore(Ev,Untold2,Stepstoconfirmthread),
                stepsinthreadtoconfirmevent(Ev,Event1,Untold1,Stepsinthreadtoconfirmevent),
                Stepstointerrupt is Stepstoconfirmthread + Stepsinthreadtoconfirmevent.

        findstepstoconfirminterruptingarc(_Arc,_CurrentArcs,_Ev,_Event,Stepstointerrupt):-
                            highnumberofsteps(Stepstointerrupt).
            
        stepstoevent([H|_T],H,1).
        stepstoevent([_H|T],Event,Steps):- 
                stepstoevent(T,Event,Steps2),Steps is Steps2 + 1.
                
        stepsinthreadtoconfirmevent(Ev1,Ev2,U,Stepsinthreadtoconfirmevent):-
            ranking(Ev1,U,Rank1),
            ranking(Ev2,U,Rank2),
            Rank1 < Rank2,
            Stepsinthreadtoconfirmevent is Rank2 - Rank1.
        stepsinthreadtoconfirmevent(_,_,_U,0).
        
        
        ranking(H,[H|_T],1).
        ranking(A,[_H|T],Rank1):-
                ranking(A,T,Rank2),Rank1 is Rank2 + 1,!.
            
        
        
        suspensealgorithm(arc(_OE,Value,Attention,_Confirmed,Confidence,_U,_T),Stepstocompletion,Stepstointerrupt,Susp):-
            imminencefunction(Stepstocompletion,Immcompletion),
            imminencefunction(Stepstointerrupt,Imminterruption),
             interruptcompletionratio(Ratio),   TotalImminence is Ratio*Immcompletion + (1-Ratio)*Imminterruption,        
             
             % %% ConfirmFactor is (1 + Confirmed)/Confidence,  
             
            /* if Ratio is 0.5 then TotalImminence is (Immcompletion + Imminterruption) / 2, */
            Susp is Value * Attention * Confidence * TotalImminence .  %% * ConfirmFactor
            /*
            
            writelist([ ' arc ',OE,' susp = ',Susp]),
            writelist([ 'val ',Value,' att ',Attention,' confpaths ',Confirmed,
                ' confid ',Confidence,' immcomp ',Immcompletion,' imminter ',Imminterruption,' total Imm ',TotalImminence]),
            nl,nl.
            */
            
            
            
            
        imminencefunction(Steps,Imminence):-
            Imminence is 1 / ( Steps ).  
            % or Imminence is e ^ - Steps.
        
        calcglobalsuspense(NewSuspenseList,GlobalSuspense):-
            min_list(NewSuspenseList,NegValencedSuspense),
            max_list(NewSuspenseList,PosValencedSuspense),
            GlobalSuspense is PosValencedSuspense - NegValencedSuspense.
        
        
        
    %******************ARC MAINTENANCE FUNCTIONS*********************
        
    maintenance(E,Arcs,Newarcs):-
    
            attentioncycle(Arcs,ArcsAtt),
            
        reevokingarcs(E,ArcsAtt,ArcsEvoked), %%
        
        %% if goal of active thread is in another thread, then make it also active 

         activatesubthreads(ArcsEvoked,ArcsEvoked,ArcsActivated),
            
            matchandshift(E,ArcsActivated,Arcs2),
            
            equalise(Arcs2,Arcs2,EqualisedArcs),
            
            disallowarcs(EqualisedArcs,EqualisedArcs,Arcs3),
            
            newarccheck(E,Arcs3,Arcs3,Arc4),
            
            toldconflicts(Arc4,Arc4,Arcs5),
            
            newpredictedarcs(Arcs5,Arcs5,Arcs6),
            
            completedarcs(Arcs6,Arcs6,Newarcs).
        
            
             
% --------------------------------% 
        
        attentioncycle([],[]).
        attentioncycle(
        [arc(OE,Value,Attention1,Confirmed,Confidence,Untold,Told)|Arcs],
        [arc(OE,Value,Attention2,Confirmed,Confidence,Untold,Told)|NewArcs]):-
        attentiondecay(Factor),
        Attention2 is Attention1 * Factor, 
        attentioncycle(Arcs,NewArcs).       
        
         % re-evoking arcs
         
        reevokingarcs(_,[],[]). %
        reevokingarcs(Ev,[arc(OE,Value,_Attention1,Confirmed,Confidence,Untold,Told)|Arcs],[arc(OE,Value,1,Confirmed,Confidence,Untold,Told)|NewArcs]):-
        member(Ev,Told), aha(Ev),
        reevokingarcs(Ev,Arcs,NewArcs). 
        reevokingarcs(Ev,[Arc|Arcs],[Arc|NewArcs]):-
        reevokingarcs(Ev,Arcs,NewArcs).
        
        
        activatesubthreads([],ArcsActivated,ArcsActivated).
        activatesubthreads([Arc|Arcs],CurrentArcs,ArcsActivated):-
            activateembeddingthreads(Arc,CurrentArcs,CurrentArcs2),
            activatesubthreads(Arcs,CurrentArcs2,ArcsActivated).
    activatesubthreads([_Arc|Arcs],CurrentArcs,ArcsActivated):-
        activateembeddingthreads(Arcs,CurrentArcs,ArcsActivated).
 
        activateembeddingthreads(_Arc,[],[]).
        activateembeddingthreads(arc(OE1,_V,1,_Confirmed1,_Confidence,Untold1,Told1),
                [arc(OE2,V2,_Att,Confirmed2,Confidence2,Untold2,Told2)|Arcs],
                [arc(OE2,V2,1,Confirmed2,Confidence2,Untold2,Told2)|Arcs2]):-
                
                OE1 \== OE2,
                aretheylinked(Untold1,Told1,Untold2,Told2),
            
            activateembeddingthreads(arc(OE1,_V,1,_Confirmed1,_Confidence,Untold1,Told1),Arcs,Arcs2).
            
        activateembeddingthreads(Arc,[H|T],[H|T2]):-
            activateembeddingthreads(Arc,T,T2).
            
            aretheylinked(Untold1,_Told1,Untold2,_Told2):-
            member(E,Untold1)   ,member(E,Untold2).
            aretheylinked(_Untold1,Told1,_Untold2,Told2):-
            member(E,Told1) ,member(E,Told2).
                
 
        matchandshift(_E,[],[]).
        matchandshift(E,
                [arc(OE,V,_Att1,Confirmed1,_Confidence,Untold,Told)|Arcs],
                [arc(OE,V,1,Confirmed2,_Confidence,NewUntold,NewTold)|Newarcs]):-
            member(E,Untold),
            Confirmed2 is Confirmed1 + 1,
            shiftover(E,Untold,NewUntold,Told,Told,NewTold),!,
            matchandshift(E,Arcs,Newarcs).
        matchandshift(E,[Arc|Arcs],[Arc|Newarcs]):-
            matchandshift(E,Arcs,Newarcs).
                
                
                
                equalise([],F,F).
                equalise([Arc|Arcs],CurrentArcs,FinalFinalArcs):-
                    equalisethreads(Arc,CurrentArcs,FinalArcs),
                     equalise(Arcs,FinalArcs,FinalFinalArcs).
                
    equalisethreads(_Arc,[],[]). 
equalisethreads(arc(OE1,V,Att1,Confirmed1,Confidence1,Untold1,Told1),[arc(OE2,V2,_Att2,Confirmed2,Confidence2,Untold2,Told2)|TailArcs],[arc(OE2,V2,1,Confirmed2New,Confidence2,NewUntold2,NewTold2)|FinalArcs]):-
                member(E,Told1),
                member(E,Untold2),
            Confirmed2New is Confirmed2 + 1,
            shiftover(E,Untold2,NewUntold2,Told2,Told2,NewTold2),           equalisethreads(arc(OE1,V,Att1,Confirmed1,Confidence1,Untold1,Told1),TailArcs,FinalArcs).
            
            equalisethreads(Arc1,[Arc2|Rest],[Arc2|Final]):-
                equalisethreads(Arc1,Rest,Final).
                
                
                
            
        newarccheck(E,CurrentArcs,Arcs,Newarcs):-
            arcdata(OE,Value,Untold),
            member(E,Untold),
            notalreadycurrent(OE,Arcs),
            shiftover(E,Untold,NewUntold,[],[],NewTold),
            % no conflicts in past assumed events or current event
             nopastconflicts(NewTold,CurrentArcs),% here we only check with previous arcs, no those new arcs added in this cycle, this is to allow ambiguous multiple interpretations of a given event to exist
            newarccheck(E,CurrentArcs,[arc(OE,Value,1,1,_Confidence,NewUntold,NewTold)|Arcs],Newarcs).
        newarccheck(_E,_C,FinalArcs,FinalArcs).                 
    
    nopastconflicts([],_Arcs).
    nopastconflicts([H|T],Arcs):-
        conflictcheckthruarcs(H,Arcs),
        nopastconflicts(T,Arcs).
    
    conflictcheckthruarcs(_H,[]).   
    conflictcheckthruarcs(H,[Arc|Arcs]):-
        \+ pastconflictcheck(H,Arc),
        conflictcheckthruarcs(H,Arcs).
        
    pastconflictcheck(H,arc(_OE,_V,_Att1,_Cfrm,_Cfdc,_U,T)):-
        member(Ev,T),
        disallow(Ev,H). % this must fail for check to succeed
        
        
        
    
    
/******************toldconflicts*****************/


        toldconflicts([],_,[]). 
        toldconflicts(
            [arc(OE,Val,Att,Confirmed,Confidence,Untold,[])|Arcs], % deal with unconfirmed arcs first, no change
            CurrentArcs,
            [arc(OE,Val,Att,Confirmed,Confidence,Untold,[])|FinalArcs]
            ):-
            toldconflicts(Arcs,CurrentArcs,FinalArcs).
            
        toldconflicts(
            [arc(OE,Val,Att,Confirmed,_Confidence,Untold,Told)|Arcs],
            CurrentArcs,
            [arc(OE,Val,Att,Confirmed,Confidence,Untold,Told)|FinalArcs]
            ):-
            checkconflicts(Told,CurrentArcs,0,Numberofconflictingarcs), 
            % now deal with confirmed arcs, find the number of past conflicting events
            confidencelevel(Numberofconflictingarcs,Confirmed,Confidence),
            toldconflicts(Arcs,CurrentArcs,FinalArcs).    
                        
                
                checkconflicts(_Told,[],FinalNumber,FinalNumber).
                checkconflicts(Told,[Arc|Arcs],N,FinalNumber):-
                    checkdisallow(Told,Arc,YesNo), % YesNo is 1 or zero
                    NextNumber is N + YesNo,
                    checkconflicts(Told,Arcs,NextNumber,FinalNumber).
                    
                checkdisallow(Events,arc(_OE,_Val,_Att,_Confirmed,_Confidence,_Untold,Told),1):-
                    member(E2,Events),
                    disallow(E1,E2),
                    member(E1,Told).
                checkdisallow(_Events,_Arc,0).
               
            
            checkintold([],_Arcs,FinalN,FinalN).
            checkintold([Ev|T],Arcs,N,FinalN):-
                    checkthru(Ev,Arcs,0,M),
                    NewN is N + M,
                    checkintold(T,Arcs,NewN,FinalN).
            
            checkthru(_Ev,[],FinalN,FinalN).
            checkthru(Ev,[arc(_OE,_Val,_Att,_Confirmed,_Confidence,_Untold,Told)|Arcs],N,FinalN):-
                member(Ev,Told),
                M is N + 1,
                checkthru(Ev,Arcs,M,FinalN).
            checkthru(Ev,[arc(_OE,_Val,_Att,_Confirmed,_Confidence,_Untold,_Told)|Arcs],N,FinalN):-
                checkthru(Ev,Arcs,N,FinalN).
                
/************ end of toldconflicts ***********************/
            
        pastconflictcheck(_,[],N,N).
        pastconflictcheck(
                AntiE,
                [arc(_OE,_Val,_Att,_Confirmed,_Confidence,_Untold,Told)|Arcs],
                Number,FinalNumber):-
                member(AntiE,Told),
                Number2 is Number + 1,
                pastconflictcheck(AntiE,
                Arcs,
                Number2,FinalNumber).
        pastconflictcheck(AntiE,
                [_Arc|Arcs],
                Number,FinalNumber):-
                pastconflictcheck(AntiE,
                Arcs,
                Number,FinalNumber).
        
        
        confidencelevel(Numberofconflictingarcs,Confirmed,Confidence):-
                conflictingandconfirmedratio(Confirmedandconflictingratio),
                Confidence is Confirmed / (Confirmed + Confirmedandconflictingratio*Numberofconflictingarcs). 
                % Confirmed = number of confirmed events in arc , 
                % Numberofconflictingarcs  =
                % number of past events which are currently disallowable
        
        
        newpredictedarcs([],A,A).
        newpredictedarcs([arc(_OE,_Value,_Att,_Confirmed,Confidence,Untold,_Told)|Arcs],
                            CurrentArcs,FinalArcs):-
            arcdata(OE2,Value2,Sequence),
            notalreadycurrent(OE2,CurrentArcs),
            nopastconflicts(Sequence,CurrentArcs), % check that no predicted events are already disallowed by another event
            notlastmember(E,Sequence),
            member(E,Untold),
            %  we give predicted arcs the same confidence level as the arc that predicted them
            newpredictedarcs([arc(OE2,Value2,1,0,Confidence,Sequence,[])|Arcs],
                [arc(OE2,Value2,1,0,Confidence,Sequence,[])|CurrentArcs],
                FinalArcs),!.
        newpredictedarcs([_Arc|Arcs],CurrentArcs,FinalArcs):-
            newpredictedarcs(Arcs,CurrentArcs,FinalArcs).
            
        
        
        notlastmember(E,[E|[_H|_T]]).
        notlastmember(E,[_H|T]):-
            notlastmember(E,T).
                
            
                    
        notalreadycurrent(_OE2,[]).
        notalreadycurrent(OE,[arc(OE2,_V,_A,_Confirmed,_Confidence,_U,_T)|Arcs]):-
            OE \== OE2,
            notalreadycurrent(OE,Arcs),!.
        
        
                
        disallowarcs([],A,A).
        disallowarcs([arc(_OE,_V,_A,_Confirmed,_Confidence,_U,T)|_Arcs],
                CurrentArcs,FinalArcs):-
                % confidencethreshold(Confidencethreshold),
                % Confidence > Confidencethreshold,
            member(E,T),
            disallow(E,AntiE),
            checkmember(AntiE,CurrentArcs,Arcs3),
            \+ CurrentArcs == Arcs3,
            disallowarcs(Arcs3,Arcs3,FinalArcs).
        disallowarcs([_Arc|Arcs],CurrentArcs,FinalArcs):-
            disallowarcs(Arcs,CurrentArcs,FinalArcs). %%%%
            
            
        completedarcs([],_Arcs5,[]).    
        completedarcs([arc(_OE,_V,_A,_Confirmed,_Confidence,[],_T)|Arcs],Arcs5,Newarcs):-
            completedarcs(Arcs,Arcs5,Newarcs).
        completedarcs([Arc|Arcs],Arcs5,[Arc|Newarcs]):-
            completedarcs(Arcs,Arcs5,Newarcs).
        
            
        
        
        checkmember(_,[],[]):-!.
        checkmember(A,[arc(_OE,_V,_Att,_Confirmed,_Confidence,U,_T)|Tail],Arcs2):-
            member(A,U),
            checkmember(A,Tail,Arcs2),!.
        checkmember(A,[arc(OE,V,Att,Confirmed,Confidence,U,T)|Tail],
                        [arc(OE,V,Att,Confirmed,Confidence,U,T)|Arcs2]):-
            checkmember(A,Tail,Arcs2),!.
            
            
        
                    
                
% ***************UTILITIES************************* 
            
            shiftover(E,[E|RestUntold],RestUntold,_Told,CurTold,FinalTold):- 
                    addtotail(E,CurTold,FinalTold),!.
            shiftover(E,[Event|RestUntold],NewUntold,Told,CurTold,FinalTold):-
                    addtotail(Event,CurTold,NewTold),
                    shiftover(E,RestUntold,NewUntold,Told,NewTold,FinalTold),!.
                
                
            size([],0).
            size([_H|T],N) :- size(T,N1), N is N1+1.

            addtotail(E,[],[E]).
            addtotail(E,[A|B],[A|C]):- 
                    addtotail(E,B,C).
                    
            getlastevent(A,[A|[]]).
            getlastevent(E,[_H|T]):-
                getlastevent(E,T).
                
                                   
            writelist([]).
            writelist([H|T]):-
                write(H),nl,writelist(T).
    
            writelist(_Feed,[]).
            writelist(Feed,[H|T]):-
                write(Feed,H),writelist(Feed,T).

            writelist2(_Feed,[]).
            writelist2(Feed,[H|T]):-
                write(Feed,H),nl(Feed),writelist2(Feed,T).
            
            writedoublelist(_,[]).
            writedoublelist([],_).
writedoublelist([arc(OE,Val,Att,Confirmed,Confidence,Untold,Told)|T1],[H2|T2]):-
                write(OE),
                nl,write('  Val: '),write(Val),write('  Att: '),format( '~2f', [Att]),
                write('  Confirmed: '),write(Confirmed),
                write('  Confidence '),format( '~2f', [Confidence]),
                nl,write(' Untold: '), write(Untold),
                nl, write(' Told: '),write(Told),
                nl,write('  suspense is '),format( '~2f', [H2]),nl,nl,
                writedoublelist(T1,T2).
                
        
        
        writeoutimportantthreads(_,[]).
        writeoutimportantthreads([],_).
writeoutimportantthreads([_Arc|T1],[H2|T2]):-
                thresholdfordisplay(Threshold),
                -Threshold < H2,
                 H2 < Threshold,
                writeoutimportantthreads(T1,T2).
                
writeoutimportantthreads([arc(OE,Val,Att,Confirmed,Confidence,Untold,Told)|T1],[H2|T2]):-               
                write(OE),
                nl,write('  Val: '),write(Val),write('  Att: '),format( '~2f', [Att]),
                write('  Confirmed: '),write(Confirmed),
                write('  Confidence '),format( '~2f', [Confidence]),
                nl,write(' Untold: '), write(Untold),
                nl, write(' Told: '),write(Told),
                nl,write('  suspense is '),format( '~2f', [H2]),nl,nl,
                writeoutimportantthreads(T1,T2).
        
       
                
        writefinallist(_,[]).
            writefinallist([],_).
            writefinallist([H1|T1],[H2|T2]):-
                write(H1),write('  => '),format( '~2f', [H2]),nl,
                writefinallist(T1,T2).
                
            writelatexlist(_,[]).
            writelatexlist([],_).
            writelatexlist(Num1,[H2|T2]):-
                write(' ( '), write(Num1),write(' , '),format( '~2f', [H2]),write(' ) '),nl,
                Num2 is Num1 + 1,
                writelatexlist(Num2,T2).    
                

  