# Make Defensible Decisions {#introchap}


```{r, include=FALSE}
require(knitr)
require(tidyverse)
require(kableExtra)
require(huxtable)

knitr::opts_chunk$set(echo = FALSE, results = "asis")

gameformat <- function(game, caption){
  gg <- as_hux(game) %>%
    set_width(ncol(game)/10) %>%
	  set_markdown() %>% 
    set_caption(caption) %>%
    set_bold(1, everywhere) %>%
    set_bold(everywhere, 1) %>%
    set_align(everywhere, everywhere, "center") %>%
    set_right_border(everywhere, 1, 0.5) %>%
    set_bottom_border(1, everywhere, 0.5) %>%
    set_right_border_color(everywhere, 1, "grey60") %>%
    set_bottom_border_color(1, everywhere, "grey60") %>%
    set_caption_pos("bottom") %>%
    set_row_height(everywhere, 0.6) %>%
  print_html(gg)
  # kbl(game, 
  #     booktabs = F, 
  #     escape = FALSE,
  #     align = paste0("r",strrep("c", ncol(game)-1)),
  #     linesep = "",
  #     caption = caption) %>%
  #   column_spec(1, 
  #               border_right = T,
  #               bold = T) %>%
  #   row_spec(0, bold = T, extra_css = "border-bottom: solid 0.5px;") %>%
  #   cell_spec(df[1, 2], extra_css = "border_left = solid 0.5px;") %>%
  #   kable_styling(full_width = F)  
}
```


A rational chooser knows what they are doing, and thinks that it is for the best. That is, they think that there is nothing else they could be doing that would be better. This book defends a version of decision theory that starts, and largely ends, with this principle. Properly understood, this is all there is to decision theory. But how to properly understand it will be the subject of much of this book.

The principle I just stated is backwards looking. It says that the chooser must think that the decision is for the best when they make it. It doesn't say much about how they come to make that decision, or whether the decision makes sense given the views the chooser has at the start of deliberation. That's by design. Decision theory is the theory of when decisions can be defended. Or, at least, that's what I'll argue in this book.

I'm mostly going to be concerned with a special class of decision problems: those involving demons who have spectacular predictive powers. These have been a particular focus of decision theorists for the last half-century. In keeping them center stage I am, in this one respect at least, following tradition. But I will make use of the principle that our theory of how to make decisions when demons are around should be consistent with our theory of how to make decisions when demons are not around. And the motivations for the two parts of the theory should be consistent as well. This turns out to be a somewhat substantive constraint.

Demons predict what other people will choose, make moves accordingly, and these moves make a difference to the consequences of other choices. That's to say, demons behave just like the rational players in orthodox game theory. Interacting with demons is, at a fairly deep level, playing games with them. So we should expect game theory to have something to tell us about how those interactions go. This isn't a novel point; I owe it to William @Harper1986. But it is going to be central to the plot of this book.

The next three sections spell out the points made in each of the last three paragraphs. And then I'll close the chapter by setting out a generic version of the main example of the book, and going over the plans for the rest of the book.




## Proceduralism and Defensivism  {#procdef}

The vast bulk of decision theories on the market are what I'll call proceduralist. That is, they provide a procedure for determining what  to do in a given problem. I'm going to argue against proceduralism, and in favor of what I'll call defensivism. And to do that, it helps to clarify just what is meant by saying that a theory provides a procedure for determining what to do. As I'll understand it, proceduralism is the conjunction of four claims about what to do in a decision problem. Two of these are about the inputs to deliberation, and two of them are about the outputs. Here are the four claims, with some hopefully useful names.

Ex Ante
:    What is to be done is a function of what things are like at the start of deliberation.

Transparency
:    The inputs to that function are all knowable as long as the chooser is sufficiently rational and self-aware.

Decisiveness
:    In any decision problem, there is only one thing which is to be done, unless there are several things that are equally good. In the latter case, adding a minimal sweetening to any one of the equally good options would make it the option to be chosen.

Possibility
:    In any finite decision problem, at least one choice is rationally permissible. That is, there are no finite rational dilemmas.

I'm going to argue against all four of these claims. The kind of theory I favor, defensivism, instead endorses the following four claims.

Ex Post
:    What is to be done is a function of what things are like at the end of deliberation.

Opacity
:    The inputs to that function involve, among other things, what probabilities are rational given the chooser's evidence, and this may be opaque to the self-aware, rational agent.

Indecisiveness
:    In many decision problems, there are permissible options which are not equally good, and there would still be many permissible options after sweetening one or other option.

Dilemmas
:    In many finite decision problems, no choice is rationally permissible. That is, there are finite rational dilemmas.

Neither Proceduralism nor Defensivism is a package deal; you can mix and match the parts. And there are many natural weakenings of one or other part of the family of views. For instance, in chapter \@ref(decisive), I'll spend some time on views that say that Decisiveness is only guaranteed to hold in cases where there are just two options.

But that said, there are natural affinities between the four parts of Proceduralism. if you thought the point of decision theory was to provide a user's guide to making decisions, you'll naturally end up with a proceduralist theory. And lots of theories have done just that. Any theory which starts with a function from states available to the chooser at the start of deliberation to numerical values, and instructs the chooser to maximise that value, will be proceduralist. That very abstract description of a decision theory covers the vast majority of theories on the market today.

Any theory of decision that assigns numerical values to each option on the basis of factors accessible to the chooser at the start of deliberation, and then exhorts the chooser to choose the option with the highest value, will be proceduralist. And if you're familiar with contemporary work in decision theory, you'll know that most theories on the market do indeed assign numerical values to each option on the basis of factors accessible to the chooser at the start of deliberation, and then exhort the chooser to choose the option with the highest value. 

In a recent paper, Adam @Elga2021 describes a class of theories he calls 'suppositional', and notes that most existing theories of decision are suppositional. (And goes on to argue that we should want a theory to be suppositional.) The suppositional theories, as he describes them, are a subset of the procedural theories. Indeed, they are a subset of the theories described in the last paragraph - those that assign numerical values to choices. So saying that most theories on the market are proceduralist is not saying anything new. If anything, it's a commonplace.

## Causal Defensivism {#cdintroduced}

It's going to take some setup to articulate precisely the positive theory I'm going to defend in this book. But I think it's worth having a rough statement of it up front, so you can see where we're headed.

According to causal defensivism, a choice is rational if the following two conditions are met.

First, at the time the choice is made, no other choice has higher expected utility, given some probability distribution over the states that is rational at that time.

Second, at the time the choice is made, no other choice weakly dominates the choice.

There are a lot of technical terms there which I'll make clearer as this chapter goes along. But the key thing 

## Basic Decision Theory {#bdt}

A simple decision problem starts with a table like this.

```{r, simple-table, cache=TRUE}
simple_table <- tribble(
	   ~"", ~`$S_1$`, ~`$S_2$`,
	   "$O_1$", "$v_{11}$", "$v_{12}$",
	   "$O_2$", "$v_{21}$", "$v_{22}$"
	)
gameformat(simple_table, "A generic 2*2 decision table")
```

On the rows we list the options that the chooser, who I'll mostly call Chooser from now on, has. On the columns we list the possible states of the world. And in the cells we list the value to Chooser of each of these option-state pairs. Just to make the notation easier to remember, I've written $v_{ij}$ for the value of the outcome when Chooser selects option $O_i$ and the world is in state $S_j$.

Now there are a lot of questions you could have about that last paragraph, and I'll spend section \@ref(tables) going over five such questions at some length. But for now let's start with that basic picture. In general there are more than two possible options and more than two possible states, but let's start with this simple case for now and work up to the more complicated cases.

In order to make a decision, Chooser typically needs one more piece of information - how likely are the states $S_1$ and $S_2$? Let's add that information in: the probability of $S_1$ is $p_1$ and the probability of $S_2$ is $y$. What do we mean by 'probability' here? Good question, and one that I'll spend a lot of time on in chapter \@ref(coherence). For now, just use its colloquial meaning.

Given all these facts, Chooser can assign a value to an option $O_i$ using the following formula:

$$
V(O_i) = p_1 v_{i1} + p_2v_{i2}
$$

And Chooser is rational iff they choose an option with maximal value.

In the more general case where there are $k$ possible states, and the probability of state $S_k$ is $p_k$, the value of option 

$$
V(O_i) = \sum_{j=1}^k p_j v_{ij}
$$

And in the even more general case where there are continuum many options, we need to use integrals to work out the value of each option. But these complexities aren't going to be particularly relevant to our story, so I'll return to the special two choice two option case, and note when the extra generalities are needed.

The formulae above are what are usually called the _expected_ values of each option, and the decision theory I've just state is that if Chooser is rational, they maximise the expected value of their choice, given this probability distribution over the states of the world. Let's restate that not using variables, but explicitly using probabilities and values. Here $Pr$ is the probability function relevant to Chooser's decision, and $V$ is the value function. I'll use concatenation for conjunction, so $O_1S_1$ means that both $O_1$ and $S_1$ are true, i.e., that the first option is chosen and the first state is actualised.

$$
V(O_i) = Pr(S_1) V(S_1 O_i) + Pr(S_2) V(S_2 O_i)
$$

I'll call the theory that values each option this way, and says that rational choosers maximise value, Basic Decision Theory. It could just as easily be called the Crude Savage Decision Theory. The 'Savage' there is because the formula at the heart of it is the same formula that @Savage1954 puts at the heart of his decision theory. But the 'Crude' is there because Basic Decision Theory leaves off all that Savage says about the nature of options and states. @SteeleSEP [sect 3.1] have a good summary of what Savage says here. I'm not going to go into that, and instead note why something needs to be said. Because as it stands, Basic Decision Theory leads to absurd outcomes.

## Why Basic Decision Theory Fails {#notbasic}

Consider the St. Crispin's Day speech that Shakespeare has Henry V give before the Battle of Agincourt. (I'm indebted to a long ago conversation with Jamie Dreier for pointing out the connection between this speech and decision theory.) The background is that the English are about to go to battle with the French, and they are heavily outnumbered. Westmoreland wants to wait for more troops, and Henry does not, offering this reasoning.

| What’s he that wishes so?
| My cousin Westmoreland? No, my fair cousin; 
| If we are marked to die, we are enough
| To do our country loss; and if to live,
| The fewer men, the greater share of  honor. 
| God’s will! I pray thee, wish not one man more.


It looks like Henry is suggesting something like the following decision table.

```{r, agincourt, cache=TRUE}
agincourt <- tribble(
	   ~"", ~`Victory`, ~`Defeat`,
	   "Attack", "$a$", "$c$",
	   "Wait", "$b$", "$d$"
	)
gameformat(agincourt, "Henry's reasoning on St. Crispin's Day")
```

Henry argues, not implausibly, that $a > b$ since they will get more honour, and $c > d$, since they will lose fewer men. Now it doesn't matter what the probabilities of victory and defeat are. To see this, call them $x$ and $y$ respectively, and note that the two facts Henry mentions suffice to guarantee that $ax + cy > bx + dy$, assuming just that the probabilities are non-negative. So great, Henry is right, and they should attack. And they do, and they win, and all's well that end's well.

No! That's an absurd decision, even if it ended well on this occasion. Note that Henry's reasoning here is completely general - you could say the same before every battle. And it isn't true that you should always rush into battle with whoever you have on hand.

At one level, it's easy to say what has gone wrong here. There is too tight a connection between the choice Henry makes, and which state of the world is actual. But, and this is the philosophical problem, just what is the problematic connection between the choice and the state? This isn't obvious, because there are two different connections between the choice and the state, and it's a matter of some philosophical import which of them matters.

On the one hand, there is an evidential connection between the choice and the state. Learning that Henry has decided to go to battle rather than wait for reinforcements is evidence that he will lose. Misleading evidence, as it turned out, but certainly evidence. And maybe what's gone wrong in Henry's reasoning is that he has ignored that evidential connection in his reasoning.

On the other hand, there is a causal connection between the choice and the state. It's possible, given Henry's evidential situation at the time he makes the decision, not just that he is defeated, but that his rushing into battle causes his defeat. Relatedly, it's possible, for all Henry knows, that rushing into battle lowered the objective chance of his winning. Those last two sentences didn't quite say the same thing, and the differences between them will matter a little going forward, but for now we'll slide over their differences, and just note that there is in some natural sense a causal connection between Henry's decision and the resulting state of the battle.

So here we get to a point of common ground among contemporary decision theorists. It will, more or less, be the last point of common ground on the journey; from here everything gets contentious. When there is both an evidential and a causal connection between the possible choices and the possible states of the world, it is inappropriate to use Basic Decision Theory to make a decision. Indeed, in these cases, Basic Decision Theory will often validate the wrong decision.

It's not quite a universal view, and we'll come back in section \@ref(quiggin) to people who don't believe it, but there is another very widely accepted claim in the vicinity of this one. When there is no evidential or causal connection between the possible choices and the possible states, most theorists think Basic Decision Theory recommends the right choice. Now they might not say, and typically do not say, that it makes the right recommendations for the right reasons. But they do say, at least most of them, that it gets the verdicts right. In the favored lingo of twentieth century philosophers, it is extensionally adequate in these cases.

So that covers the cases where there is both an evidential and causal connection - Basic Decision Theory gets things wrong - and the cases where there is neither - Basic Decision Theory gets things right. But what about the cases where there is one such connection but not the other? We're all taught that correlation is not causation. What happens when there is correlation but not causation between the choices and the states? Then things get really interesting, and that's the debate we're doing to jump into.


## Newcomb Problems {#newcombproblem}


It's not at all obvious how there could be a case where the possible choices and possible states could be causally connected but not evidentially connected. I'm going to set the possibility of such a case aside, at least until someone shows me what such a case might look like. Because there is a very natural way that the choices and states could be evidentially but not causally connected: they could have a common cause. And one way that could come about is if the states are predictions of Chooser's choice, made by someone who has a deep insight into Chooser's choice dispositions.

We'll call that someone Demon, and a decision problem in which the states are based in Demon's predictions a Demonic decision problem. I'll have much more to say about demons in section \@ref(aboutademon), but for now all we need to know is that Demon has means, motive, and opportunity to correctly predict what strategy a Chooser will adopt.

The most famous Demonic decision problem is _Newcomb's Problem_ [@Nozick1969]. Chooser is presented with two boxes, one opaque and one clear. They have a choice between taking just the opaque box, i.e., taking one box, and taking both the opaque and the clear box, i.e., taking two boxes. The clear box has a good of value $y$ in  it. The contents of the opaque box are unknown. Demon has predicted the chooser's choice, and has placed a good of value $x$ in it if they predict Chooser will take one box, and left it empty (which we'll assume has value 0) if they predict Chooser will take both boxes. The key constraint is $x > y$. In most versions the value given for $x$ is massively greater than that for $y$, but the theories that are developed for the problem typically are sensitive only to whether $x$ is larger than $y$, not to how much larger it is.

Demon is really good at their job. They are not a time traveller; they are making a prediction that is not causally influenced by what the Chooser actually does. But they are really good. I'll assume that they are arbitrarily good, and come back to just what I mean by that in section \@ref(aboutademon).

I'll write 1 and 2 for the two choices, and P1 and P2 for the predictions. In general, where there is a demon who makes these kinds of predictions, I'll write 'PX' to mean the state of choice X being predicted. So here is the decision problem.

```{r, general-newcomb, cache=TRUE}
general_newcomb <- tribble(
	   ~"", ~P1, ~P2,
	   "1", "$x$", "0",
	   "2", "$x + y$", "$y$"
	)
gameformat(general_newcomb, "Newcomb's Problem")
```

A large part of late 20th Century decision theory was given over to discussing this problem. So-called Causal Decision Theorists argued in favor of taking both boxes. The primary argument is that whatever the demon has done, the chooser gets a bonus of $y$ for taking the second box. It's good to get guaranteed bonuses, so they should take the bonus. This is basically the view I'm going to defend in this book, though with a number of deviations from the way it was defended in these classic works. So-called Evidential Decision Theorists argued in favor of taking just the one box. The primary argument is that the chooser who takes one box expects to get $x$, the chooser who expects to get both boxes expects to get $y$, it's better to take a choice that one expects to do better, and $x > y$, so it's better to take one box. Insert citations to each side

## Two Ways to Extend Basic Decision Theory {#edtcdt}

The two most famous theories in recent work in decision theory are Causal Decision Theory and Evidential Decision Theory. I introduced these terms in section \@ref(newcombproblem) without defining them. It's time to do that now.

As I understand the way the terms are used, and indeed as I'll be using them, they are potentially misleading. Both of these are not really theories, but families of theories. Evidential Decision Theory (EDT) is a somewhat tighter family of theories than Causal Decision Theory (CDT), but neither is something that I would typically be happy calling a theory. In this section I'll give somewhat imprecise descriptions of each 'theory', starting with EDT. In section \@ref(edtcdtprecise) I'll say why both of these are really theory schema, and set out some of the more viable ways of making them into precise theories.

EDT, as we're going to understand it, traces back to the first edition of Richard Jeffrey's _The Logic of Decision_ [@Jeffrey1965]. The idea behind it is that what goes wrong with Henry's reasoning at Agincourt is that he ignores the fact that rushing into battle lowers the probability that he will win. In fact, according to EDT, that probability doesn't really matter to his decision. What matters is the probability that he will win if he attacks, and the probability that he will win if he waits for reinforcements. The value of each choice, according to EDT, is given by this formula

$$
V(O_i) = Pr(S_1 | O_i) V(S_1 O_i) + Pr(S_2 | O_i) V(S_2 O_i)
$$

And, as before, the one rule in decision theory is that one should maximise value.

So EDT says that Basic Decision Theory is incorrect. It uses probabilities of states, i.e., terms like $Pr(S_j)$, where it should use probabilities of states given choices, i.e., terms like $Pr(S_j | O_i)$. That's what goes wrong with Henry's reasoning.

In Newcomb's Problem, EDT says that one should take one box. Assume, for simplicity, that the probability that the demon will make correct predictions is 1. Then the value of taking one box is $x$, the value of taking two boxes is $y$, and by hypothesis $x >>> y$, so one should take one box.

CDT, or at least the version we're going to focus on for a while, traces back to David Lewis's paper _Causal Decision Theory_ [@Lewis1981b]. Lewis actually has two aims in this paper: to set out a version of CDT, and to argue that the other versions don't differ in significant ways from his version. It's going to be somewhat important to the plotline of this book that Lewis's second claim, that the various versions of CDT don't greatly differ, is false. But the positive theory Lewis presents is interesting whether or not that second claim goes through, and that's what we'll focus on.

The idea is that Basic Decision Theory was not incorrect, as EDT says, but incomplete. It needs to be supplemented with rules about when the formula can be applied. In particular, we need to add that the states have to be causally independent of the options. In Lewis's terminology, the states have to be 'dependency hypotheses', that state how the outcomes of the choice depend upon the option Chooser selects. If you apply the formula to cases where the states themselves depend (or even may depend) on the option, things go wrong. That's what CDT says goes wrong in Henry's case. He applies the formula correctly, but he shouldn't have started with simply Win and Lose as the states, since those states depend, causally, upon his choice.

In Newcomb's Problem, CDT says that one should take one box. What Demon predicts is not causally dependent on what Chooser selects. So we can use P1 and P2 as states. Let $z$ be the probability of P1, and hence the probability of P2 is $1-z$. Then the expected value of taking one box is $zx$, while the expected value of taking two boxes is $zx + y$. Without yet knowing what $z$ is, a question that will become rather important as we go on, we know that $zx + y > zx$, so taking two boxes has higher value. So that's what one should do.





## Making The Theories Precise {#edtcdtprecise}

So that's the basic picture of EDT and CDT. But as I alluded to earlier, setting out the basic picture isn't quite the same thing as setting out a theory. In this section I'll flag some factors that need to be settled 

## Defensivism {#defensivism}

- Given probabilities after decision making, the view makes sense (by causal standards)
- Not weakly dominated
- This is also a post-decision rule

## Newcomb Games {#newcombgames}

The example involving the friend can be used to generate an interesting variant of Newcomb's Puzzle, due to Frank @Arntzenius2008. Keep the contents of the boxes the same, including that the demon puts $x$ into the first box iff the demon predicts only one box will be taken. But this time both boxes are clear. Now the chooser has the same view as the well-meaning friend. What should they do?

We can model this problem using the following decision tree.

```{tikz, transparent-Newcomb, fig.cap = "Newcomb's Problem with both boxes transparent", fit.ext = 'png', cache=TRUE}
\begin{tikzpicture}[scale=1.5,font=\footnotesize]
\tikzset{
% Two node styles for game trees: solid and hollow
solid node/.style={circle,draw,inner sep=1.5,fill=black},
hollow node/.style={circle,draw,inner sep=1.5},
square node/.style={rectangle,draw,inner sep=1,fill=black}
}
% Specify spacing for each level of the tree
\tikzstyle{level 1}=[level distance=10mm,sibling distance=25mm]
\tikzstyle{level 2}=[level distance=10mm,sibling distance=15mm]
\tikzstyle{level 3}=[level distance=15mm,sibling distance=15mm]
\tikzstyle arrowstyle=[scale=1]
\tikzstyle directed=[postaction={decorate,decoration={markings,
mark=at position .5 with {\arrow[arrowstyle]{stealth}}}}]
% The Tree
\node(0)[hollow node,label=above:{$Demon$}]{}

child{node(2)[solid node, label=left:{$Chooser$}]{}
child{node[square node,label=below:{$x$}]{}edge from parent node[left]{1} }
child{node[square node,label=below:{$x+y$}]{} edge from parent node[right]{2}}
edge from parent node[left,yshift = 5]{P1}}
child{node(3)[solid node, label=right:{$Chooser$}]{}
child{node[square node,label=below:{$0$}]{}edge from parent node[left]{1} }
child{node[square node,label=below:{$y$}]{} edge from parent node[right]{2}}
edge from parent node[right,yshift = 5]{P2}
}
;
\end{tikzpicture}
```


This representation should look familiar from game theory textbooks. It's just a standard extensive form representation of a game where each player makes one move. Since we'll be using trees like this a bit, I want to explain the notation. It's mostly standard within game theory, but since not all philosophers are game theorists, it's worth going a bit carefully.

The game starts at the hollow node, which in this case is at the top of the tree. At each node, we move along a path from wherever we are to a subsequent node. So each node gets labeled with who is making the choice, and the edges get labeled with the choices they can make. This game starts with the Demon predicting either that Chooser takes 1 box - this is the edge labeled P1 - or that Chooser takes 2 boxes. Either way we get to a node where Chooser moves, either by taking 1 box or 2. It's a solid node, which means (in the notation of this book) that it's not where the game starts, and it's not where the game ends. Then whatever happens, we get to a terminal node, here denoted with a square. At each terminal node we list the payouts. 

But here we only listed the payouts to Chooser. To make something really into a game, there should be payouts for both players. What are Demon's payouts? Well, what makes something the payout function for a player is that it takes higher values the more they get what they want. Since Demon is trying to predict player, they want situations where they predict them well. So we can simply say that their payout is 1 for a correct prediction, and 0 for an incorrect prediction. That suggests the tree for Arntzenius's 'transparent box' version of Newcomb's Problem should look like this.

```{tikz, transparent-Newcomb-two-payouts, fig.cap = "Newcomb's Problem with both boxes transparent, and Demon's payouts listed", fit.ext = 'png', cache=TRUE}
\begin{tikzpicture}[scale=1.5,font=\footnotesize]
\tikzset{
% Two node styles for game trees: solid and hollow
solid node/.style={circle,draw,inner sep=1.5,fill=black},
hollow node/.style={circle,draw,inner sep=1.5},
square node/.style={rectangle,draw,inner sep=1,fill=black}
}
% Specify spacing for each level of the tree
\tikzstyle{level 1}=[level distance=10mm,sibling distance=25mm]
\tikzstyle{level 2}=[level distance=10mm,sibling distance=15mm]
\tikzstyle{level 3}=[level distance=15mm,sibling distance=15mm]
\tikzstyle arrowstyle=[scale=1]
\tikzstyle directed=[postaction={decorate,decoration={markings,
mark=at position .5 with {\arrow[arrowstyle]{stealth}}}}]
% The Tree
\node(0)[hollow node,label=above:{$Demon$}]{}

child{node(2)[solid node, label=left:{$Chooser$}]{}
child{node[square node,label=below:{$x,1$}]{}edge from parent node[left]{1} }
child{node[square node,label=below:{$x+y,0$}]{} edge from parent node[right]{2}}
edge from parent node[left,yshift = 5]{P1}}
child{node(3)[solid node, label=right:{$Chooser$}]{}
child{node[square node,label=below:{$0,0$}]{}edge from parent node[left]{1} }
child{node[square node,label=below:{$y,1$}]{} edge from parent node[right]{2}}
edge from parent node[right,yshift = 5]{P2}
}
;
\end{tikzpicture}
```

I've put Demon's payouts second, even though Demon moves first. The focus here is on Chooser, so they are player 1. When a game representation lists the payout in a situation as $a, b$ that means that player 1 gets $a$ and player 2 gets $b$. In this case that means the chooser gets $a$ and the demon gets $b$. 

In this book I'm mostly going to work with games where Demon's payouts are either 1 for a correct prediction of 0 for an incorrect one. But once we've got the basic concept of Demon as a player getting payouts, we can set the demon up with other payouts too. And then we can bring just about any tool we like from contemporary game theory to bear on demonic decision theory. 

That move, of treating Newcomb Problems as games, is taken straight from work by William @Harper1986. And it is going to be the central move in this book.

Here is what the original Newcomb problem looks like when we follow Harper's lead and transform it into a game.

```{r}
general_newcomb <- tribble(
	   ~"", ~P1, ~P2,
	   "1", "$x, 1$", "$0, 0$",
	   "2", "$x + y, 0$", "$y, 1$"
	)
gameformat(general_newcomb, "Newcomb's Problem as a Game")
```

Or, at least, that's the so-called strategic form of the game. We can also represent it as a game that takes place over time, like this.

```{tikz, hidden-Newcomb-two-payouts, fig.cap = "Newcomb's Problem with first box hidden, and Demon's payouts listed", fit.ext = 'png', cache=TRUE}
\usetikzlibrary{calc} 
\begin{tikzpicture}[scale=1.5,font=\footnotesize]
\tikzset{
% Two node styles for game trees: solid and hollow
solid node/.style={circle,draw,inner sep=1.5,fill=black},
hollow node/.style={circle,draw,inner sep=1.5},
square node/.style={rectangle,draw,inner sep=1,fill=black}
}
% Specify spacing for each level of the tree
\tikzstyle{level 1}=[level distance=10mm,sibling distance=25mm]
\tikzstyle{level 2}=[level distance=10mm,sibling distance=15mm]
\tikzstyle{level 3}=[level distance=15mm,sibling distance=15mm]
\tikzstyle arrowstyle=[scale=1]
\tikzstyle directed=[postaction={decorate,decoration={markings,
mark=at position .5 with {\arrow[arrowstyle]{stealth}}}}]
% The Tree
\node(0)[hollow node,label=above:{$Demon$}]{}

child{node(2)[solid node]{}
child{node[square node,label=below:{$x,1$}]{}edge from parent node[left]{1} }
child{node[square node,label=below:{$x+y,0$}]{} edge from parent node[right]{2}}
edge from parent node[left,yshift = 5]{P1}}
child{node(3)[solid node]{}
child{node[square node,label=below:{$0,0$}]{}edge from parent node[left]{1} }
child{node[square node,label=below:{$y,1$}]{} edge from parent node[right]{2}}
edge from parent node[right,yshift = 5]{P2}
}
;
\draw[dashed](2) to (3);
\node at ($(2)!.5!(3)$)[above]{$Chooser$};
\end{tikzpicture}
```

The dashed line there represents that those two nodes are in what game theorists call at information set. That means that when the player to move reaches one of those nodes, all they know is that they are at one of these nodes and not any other. In this case, Chooser knows that they have to select 1 box or 2, and they know the payouts given their choice and Demon's prediction. But they do not know what Demon predicted, so they do not know which node they are at.

This extensive form representation is in a way more accurate than the strategic form representation in the table above. It encodes that Demon goes first, which is something usually stressed in the story that is told about Newcomb's Problem. But the table form is easier to read, and makes clearer that there is only one equilibrium of the game: Demon makes prediction P2 and Chooser chooses 2. So I'll mostly use tables when they are possible. And they often are possible - lots of games can be turned into demonic decision problems like Newcomb's Problem.

## Familiar Games {#familiar}

Much of what happens in this book comes from seeing demonic decision problems as games and, conversely, seeing games as potential demonic decision problems. So I want to spend a little time setting out how the translation between the two works. 

Transforming a demonic decision problem into a game is easy. As I noted, you just replace the states generated by Demon's choices with moves for Demon, and give them payout 1 if they predict correctly, and 0 otherwise. 

You might worry that this only gives you cases where Demon is approximately perfect, but we also want cases where the demon is, say, 80% accurate. But that's easy to do as well. In fact there are two ways to do it.

The first is what I'll call the Selten strategy, because it gives the demon a 'trembling hand' in the sense of @Selten1975. Instead of letting Demon choose a state in the original problem, let Demon choose one of $n$ buttons, where $n$ is the number of choices the (human) chooser has. Each button is connected to a probabilistic device that generates one of the original states. If you want Demon to be 80% accurate, say the button $b_i$ associated with option $o_i$ outputs state $s_i$ with probability 0.8, and each of the other states with probability $\frac{0.2}{n - 1}$. And still say that Demon gets payout 1 for any $i$ if the chooser selects $o_i$ and the button generates state $s_i$, and 0 otherwise. 

The second is what I'll call the Smullyan strategy, because it involves a Knights and Knaves puzzle of the kind that play a role in several of his books, especially @Smullyan1978. Here the randomisation takes place before Demon's choice. Demon is assigned a type Knight or Knave. Demon is told of the assignment, but Chooser is not. If Demon is assigned type Knight, the payouts stay the same as in the game where Demon is arbitrarily accurate. If Demon is assigned type Knave, the payouts are reversed, and Demon gets payout 1 for an incorrect prediction.

There are benefits to each approach, and there are slightly different edge cases that are handled better by one or other version. I'm mostly going to stick to cases where Demon is arbitrarily accurate, but I need these on the table to talk about cases others raise where Demon is only 75-80% accurate. And in general either will work for turning a demonic decision problem into a game.

Turning games into demonic decision problems is a bit more interesting. Start with a completely generic two-player, two-option, simultaneous move, symmetric game. We won't only look at symmetric games, but it's a nice way to start.

```{r,basic-sym-game, cache=TRUE}
symmetric_game <- tribble(
	   ~"", ~a, ~b,
	   "a", "$x, x$", "$y, z$",
	   "b", "$z, y$", "$w, w$"
	)
gameformat(symmetric_game, "A generic symmetric game")
```

In words, what this says is that each player chooses either $a$ or $b$. If they both choose $a$, they both get $x$. If they both choose $b$, they both get $w$. And if one chooses $a$ and the other chooses $b$, the one who chooses $a$ gets $y$ and the one who chooses $b$ gets $z$. (Note that the payouts list row's payment first, if you're struggling to translate between the table and the text.) A lot of famous games can be defined in terms of restrictions on the four payout values. For example, a game like this is a Prisoners' Dilemma if the following constraints are met.

- $x > z$
- $y > w$
- $w > x$

Some books will also add $2x > y + z$ as a further constraint, but I'll stick with these three.

Now to turn a game into a demonic decision problem, first replace column's payouts with 1s and 0s, with 1s along the main diagonal, and 0s everywhere else. So our generic symmetric game will now look like this.

```{r,demon-sym-game,cache=TRUE}
demonic_symmetric_game <- tribble(
	   ~"", ~a, ~b,
	   "a", "$x, 1$", "$y, 0$",
	   "b", "$z, 0$", "$w, 1$"
	)
gameformat(demonic_symmetric_game, "The demonic version of a generic symmetric game")
```

And then replace Demon's moves with states that are generated by Demon's predictions. As before, I'll put 'P' in front of a choice name to indicate the state of that choice being predicted. So our generic symmetric game looks like this.

```{r,gen-dem-problem, cache=TRUE}
generic_demonic_problem <- tribble(
	   ~"", ~pa, ~pb,
	   "a", "$x$", "$y$",
	   "b", "$z$", "$w$"
	)
gameformat(generic_demonic_problem, "The demonic decision problem generated by a generic symmetric game")
```

And note if we add the constraints $x > z, y > w, w > x$, this is essentially a Newcomb Problem. I'm a long way from the first to point out the connections between Prisoners' Dilemma and Newcomb's Problem; it's literally in the title of a David Lewis paper [@Lewis1979e]. But what I want to stress here is the recipe for turning a familiar game into a demonic problem.

We can do the same thing with Chicken. The toy story behind Chicken is that two cars are facing off at the end of a road. They will drive straight at each other, and at the last second, each driver will choose to swerve off the road, which we'll call option $a$, or stay on the road, which we'll call option $b$. If one swerves and the other stays, the one who stays is the winner. If they both swerve they both lose and it's boring, and if they both stay it's a fiery crash. So in terms of the payouts in the general symmetric game, the constraints are:

- $x < z$
- $y >> w$
- $x >> w$

Just what it means for one value to be much more than another, which is what I mean by '$>>$', is obviously vague. But let's put some example numbers in that seem like they should satisfy it.

```{r, basic-chicken, cache=TRUE}
basic_chicken <- tribble(
	   ~"", ~a, ~b,
	   "a", "$0, 0$", "$0, 1$",
	   "b", "$1, 0$", "$-100, -100$"
	)
gameformat(basic_chicken, "A version of Chicken")
```

Now replace the other driver, the one who plays column in this version, with a Demon, who only wants to predict row's move.

```{r,demon-chicken, cache=TRUE}
demon_chicken <- tribble(
	   ~"", ~a, ~b,
	   "a", "$0, 1$", "$0, 0$",
	   "b", "$1, 0$", "$-100, 1$"
	)
gameformat(demon_chicken, "A demonic version of Chicken")
```

All I've done is replace column's payouts with 1s on the main diagonal, and 0s elsewhere. And the next step is to replace the demonic player with states generated by Demon's choices, which gives us this decision problem.

```{r,egan-game, cache=TRUE}
egan_game <- tribble(
	   ~"", ~pa, ~pb,
	   "a", "$0$", "$0$",
	   "b", "$1$", "$-100$"
	)
gameformat(egan_game, "A demonic decision problem based on Chicken")
```

And that's just the Psychopath Button example that Andy @Egan2007 raises as a problem for Causal Decision Theory.

Another familiar game from introductory game theory textbooks is Matching Pennies. This is a somewhat simplified version of rock-paper-scissors. Each player has a penny, and they reveal their penny simultaneously. They can either show it with the heads side up (option $a$), or the tails side up (option $b$). We specify in advance who wins if they show the same way, and who wins if they show opposite ways. So let's say column will win if both coins are heads or both are tails, and row will win if they are different. Then we get the following game.

```{r,match-pennies, cache=TRUE}
matching_pennies <- tribble(
	   ~"", ~a, ~b,
	   "a", "$0, 1$", "$1, 0$",
	   "b", "$1, 0$", "$0, 1$"
	)
gameformat(matching_pennies, "Matching Pennies")
```

This isn't a symmetric game, but it is already demonic. Column's payouts are 1 in the main diagonal and 0 elsewhere. So we can convert it to a demonic decision problem fairly easily.

```{r,death-in-damascus, cache=TRUE}
d_i_d <- tribble(
	   ~"", ~pa, ~pb,
	   "a", "$0$", "$1$",
	   "b", "$1$", "$0$"
	)
gameformat(d_i_d, "Matching pennies as a decision problem")
```

And this the familiar problem Death in Damascus from @GibbardHarper1978.

Let's do one last one, starting with the familiar game Battle of the Sexes. Row and Column each have to choose whether to do $r$ or $c$. They both prefer doing the same thing to doing different things. But Row would prefer they both do $r$, and Column would prefer they both do $c$. (The original name comes from a version of the story where Row and Column are a heterosexual married couple, and Row wants to do some stereotypically male thing, while Column wants to do some stereotypically female thing. That framing is tiresome at best, but the category of asymmetric coordination games is not, hence my more abstract presentation.) So here's how we might think of the payouts.

```{r,bach-stravinsky, cache=TRUE}
b_o_t_s <- tribble(
	   ~"", ~r, ~c,
	   "r", "$4, 1$", "$0, 0$",
	   "c", "$0, 0$", "$1, 4$"
	)
gameformat(b_o_t_s, "Battle of the sexes")
```

As it stands, that's not a symmetric game. But we can make it a symmetric game by relabeling the choices. Let option $a$ for each player be doing their favored choice, and option $b$ be doing their less favored choice. Then the table looks like this.

```{r,bach-stravinsky-symmetric, cache=TRUE}
b_o_t_s_symmetric <- tribble(
	   ~"", ~a, ~b,
	   "a", "$0, 0$", "$4, 1$",
	   "b", "$1, 4$", "$0, 0$"
	)
gameformat(b_o_t_s_symmetric, "Battle of the sexes, relabeled")
```

Now change column's payouts so that it is a demonic game.

```{r,bach-demon, cache=TRUE}
b_o_t_s_demonic <- tribble(
	   ~"", ~a, ~b,
	   "a", "$0, 1$", "$4, 0$",
	   "b", "$1, 0$", "$0, 1$"
	)
gameformat(b_o_t_s_demonic, "A demonic version of battle of the sexes")
```

And now replace Demon's choices with states generated by (probably accurate) predictions.

```{r,asymm-death-damascus, cache=TRUE}
asymm_d_i_d <- tribble(
	   ~"", ~pa, ~pb,
	   "a", "$0$", "$4$",
	   "b", "$1$", "$0$"
	)
gameformat(asymm_d_i_d, "A demonic decision problem based on battle of the sexes")
```

And this is the asymmetric version of Death in Damascus from @Richter1984.

The point of this section has not just been to show that we can turn games into decision problems by treating one of the players as a predictor. That's true, but not in itself that interesting. Instead I want to make two further points. 

One is that most of the problems that have been the focus of attention in the decision theory literature in the past couple of generations can be generated from very familiar games, the kinds of games you find in the first one or two chapters of a game theory textbook, this way. 

The second point is that most of the simple games you find in those introductory chapters turn out to result, once you transform them this way, in demonic decision problems that have been widely discussed. But there is just one exception here. There hasn't been a huge amount of discussion of the demonic decision problem you get when you start with Stag Hunt. Let's turn to that in the next section. 

In later parts of the book, I'll be frequently appealing to decision problems that are generated from other games that have been widely discussed by economic theorists. Most of these discussions are not particularly recent; the bulk of the work I'll be citing is from the 1980s and 1990s, and I don't take myself to be making a significant contribution to contemporary economic theorising. But what I want to point out is that there is a vast source of examples in the economic theory literature that decision theorists could be, and should be, discussing. And I've spent so long here on the translation between the two literatures in part because I think there are huge gains to be had from bringing these literatures into contact.

## An Indecisive Example {#indecisive}

This section is mostly going to be talking about games that are commonly known as Stag Hunts. Brian Skyrms has written extensively on why Stag Hunts are philosophically important [@Skyrms2001, @Skyrms2004], and putting them at the center of the story is one of several ways in which this book is following Skyrms's lead.

Stag Hunts are symmetric two-player two-option, simultaneous move games. So they can be defined by putting constraints on the values in \@ref(tab:basic-sym-game). In this case, the constraints are

- $x > z$
- $w > y$
- $x > w$
- $z + w > x + y$

The name comes from a thought experiment in Rousseau's _Discourse on Inequality_.

> They were perfect strangers to foresight, and were so far from troubling themselves about the distant future, that they hardly thought of the morrow. If a deer was to be taken, every one saw that, in order to succeed, he must abide faithfully by his post: but if a hare happened to come within the reach of any one of them, it is not to be doubted that he pursued it without scruple, and, having seized his prey, cared very little, if by so doing he caused his companions to miss theirs.  [@Rousseau1913 209--10]

Normally, option $a$ is called Hunting, and option $b$ is called Gathering. The game has two equilibria - both players Hunt, or both players Gather. So it's unlike Prisoners' Dilemma, which only has one equilibria. And the more cooperative equilibria, where both players Hunt, is better. But, and this is crucial, it's a risky equilibria. To connect it back to Rousseau, the thought is that the players would both be better off if they both cooperated to catch the stag (or deer in this translation). But cooperating is risky; if the players do different things, it is best to go off gathering berries (or bunnies) on one's own than trying in vain to catch a stag single-handed.

And that's what we see in the game. The first two constraints imply that the game is in equilibrium if the players do the same thing. The third constraint says that if they both Hunt, option $a$, they are better off than if they both Gather, option $b$. But the fourth constraint codifies the thought that this is a risky equilibrium. Even though the equilibrium where everyone Hunts is better, there are multiple reasons we might end up at the equilibrium where everyone Gathers.

One reason for this is that the players might want to minimise regret. Each play is a guess that the other player will do the same thing. If one plays $a$ and guesses wrong, one loses $w - y$ compared to what one could have received. If one plays $b$ and guesses wrong, one loses $x - z$. And the last constraint entails that $x - z \< w - y$. So playing $b$ minimises possible regret.

Second, one might want to maximise expected utility, given uncertainty about what the other player will do. Since one has no reason to think the other player will prefer $a$ to $b$ or vice versa - both are equilibria - maybe one should give each of them equal probability. And then it will turn out that $b$ is the option with highest expected utility. Intuitively, $b$ is a risky option and $a$ is a safe option, and when in doubt, perhaps one should go for the safe option.

We can turn Stag Hunt into a decision problem by replacing the other player with  Demon in a way that should be familiar by now. So we get this decision problem.

```{r,stag-decision, cache=TRUE}
stag_decision <- tribble(
	   ~"", ~pa, ~pb,
	   "a", "$x$", "$y$",
	   "b", "$z$", "$w$"
	)
gameformat(stag_decision, "A demonic decision problem based on Stag Hunt")
```

In order to have less algebra, I'm going to often focus mostly on a particular version of this decision, with the following values. But it's important that the  main conclusions will be true of all decision problems based on Stag Hunt.

```{r,stag-decision-particular, cache=TRUE}
stag_decision_particular <- tribble(
	   ~"", ~pa, ~pb,
	   "a", "6", "0",
	   "b", "4", "3"
	)
gameformat(stag_decision_particular, "A particular version of a demonic decision problem based on Stag Hunt")
```

These kinds of decisions are important in the history of game theory because they illustrate in the one game the two most prominent theories of equilibrium selection: risk dominance and payoff dominance [@HarsanyiSelten1988]. Risk dominance recommends gathering; payoff dominance recommends hunting. And most contemporary philosophical proponents of decisive decision theories (in the sense of decisiveness described back in section \@ref(procdef))  fall into one of these two camps.

In principle, there are three different views that a decisive theory could have about Stag Decisions: always Hunt, always Gather, or sometimes do one and sometimes the other. A decisive theory has to give a particular recommendation on any given Stag Decision, but it could say that the four constraints don't settle what that decision should be. Still, in practice all existing decisive theories fall into one or other of the first two categories.

One approach, endorsed for rather different reasons by Richard @Jeffrey1983 and Frank @Arntzenius2008, says to Hunt because it says in decisions with multiple equilibria, one should choose the equilibria with the best payout. Evidential Decision Theorists also say to Hunt in these situations, because the all-Hunt outcome is better than the all-Gather outcome, and it doesn't even matter whether these are equilibria. Another family of approaches says to always Gather in Stag Decisions. For very different reasons, this kind of view is endorsed by Ralph @Wedgwood2013, Dmitri @Gallow2020 and Abelard @Podgorski2022. These three views differ from each other in how they motivate Gathering, and in how they extend the view to other choices, but they all agree that one should Gather in any Stag Decision.

I'm going to argue that all of these views are mistaken. Decision Theory should not say what to do in these cases - either choice is rational.

Now I should note here that I'm slightly cheating in setting out the problem this way. The theory I defend says that in any decision problem like this with two equilibria, either choice can be rational. And that includes games like, say, this one, where everyone I mentioned in the last few paragraphs would agree that $a$ is the uniquely correct choice.

```{r,not-stag-decision, cache=TRUE}
not_stag_decision <- tribble(
	   ~"", ~pa, ~pb,
	   "a", "6", "0",
	   "b", "2", "3"
	)
gameformat(not_stag_decision, "A multiple equilibrium decision problem that is not a Stag Hunt")
```

I certainly don't want to lean too hard on the intuition that either option is rational in Stag Hunts - though I do in fact think that it's intuitive that either option is rational in Stag Hunts. But if we were just leaning on intuitions, then this example would be devastating to my theory, since it really isn't particularly intuitive here that either option is rational. Thankfully, the argument, which I'll set out in some detail in chapter \@ref(decisive), doesn't appeal to these kinds of intuitions. Still, I think it's useful to focus on Stag Hunts because, as Skyrms shows, they are so philosophically important. And they will be my canonical example of a problem where the right decision theory is Indecisive.

## An Example of a Dilemma {#firstdilemma}

- Rock Paper Scissors

## The Main Example

```{tikz, main-example, fig.cap = "Generic version of the main example", fig.ext = 'png', cache=TRUE}
\usetikzlibrary{calc}
\begin{tikzpicture}[scale=1.5,font=\footnotesize]
\tikzset{
% Two node styles for game trees: solid and hollow
solid node/.style={circle,draw,inner sep=1.5,fill=black},
hollow node/.style={circle,draw,inner sep=1.5}
}
% Specify spacing for each level of the tree
\tikzstyle{level 1}=[level distance=10mm,sibling distance=25mm]
\tikzstyle{level 2}=[level distance=10mm,sibling distance=25mm]
\tikzstyle{level 3}=[level distance=15mm,sibling distance=15mm]
\tikzstyle arrowstyle=[scale=1]
\tikzstyle directed=[postaction={decorate,decoration={markings,
mark=at position .5 with {\arrow[arrowstyle]{stealth}}}}]
% The Tree
\node(0)[solid node,label=above:{$Demon$}]{}

child{node(1)[hollow node, label=below:{$(x_5,y)$}]{}
edge from parent node[left,xshift=-3]{Out}
}
child{node(2)[solid node,label=right:{$Demon$}]{}
child{node(3)[solid node]{}
child{node[hollow node,label=below:{$(x_1,1)$}]{}edge from parent node[left]{A} }
child{node[hollow node,label=below:{$(x_2,0)$}]{} edge from parent node[right]{B}}
edge from parent node[left,xshift = -3]{PA}}
child{node(4)[solid node]{}
child{node[hollow node,label=below:{$(x_3,0)$}]{}edge from parent node[left]{A} }
child{node[hollow node,label=below:{$(x_4,1)$}]{} edge from parent node[right]{B}}
edge from parent node[right,xshift = 3]{PB}
}
edge from parent node[right,xshift=3]{In}
};
% information set
\draw[dashed,rounded corners=10]($(3) + (-.2,.25)$)rectangle($(4) +(.2,-.25)$);
% specify mover at 2nd information set
\node at ($(3)!.5!(4)$) {$Chooser$};
\end{tikzpicture}
```
## Plan for the Book {#Plan}

# Why So Defensive? {#defensive}

I'm arguing for causal defensivism. And much of the argument will come in the next three chapters, when I argue in turn against three of the component parts of of proceduralism. But in this chapter I want to first address an argument that proceduralism must be right, because only procedural theories can deliver what decision theory promises: a rule for making decisions. And the main argument of this chapter is going to be that decision theory cannot, and should not, be in the business of providing such a rule. Such a rule would have to be sensitive to resource constraints, and this kind of sensitivity isn't compatible with doing the kind of theorising that decision theorists do.

## Decision Theory and Making Decisions {#algorithm}

We'll get to why I think decision theory isn't, and shouldn't be, used to help humans make decisions. First I want to argue against something that is perhaps less widely believed, but it probably more plausible: that decision theory will be a helpful way for machines who are not subject to serious resource constraints to make decisions.

We're currently engaging in a massive project of making machines that make decisions, from 'smart' thermostats to self-driving cars. Now one might have hoped that decision theory would have something useful to contribute to this project. That hope I think can be realised, but it's complicated. One might have further hoped that decision theory would be helpful in a way that only a proceduralist theory can be helpful, by providing an algorithm to program into the machines. And that hope that some might have won't be, and shouldn't be, met. That's because sometimes we want the machines to be irrational. Here is one simple case of this, based in part on David Lewis's work on nuclear deterrence [@Lewis1989c], and in part on _Dr. Strangelove_ [@Kubrick1964].

Chooser is President of a relatively small country. Due to an unfortunate machine translation incident with their larger neighbor during widget tariff negotiations, the neighbor has become an Enemy. And this Enemy now plans to express their displeasure by launching a nuclear missile at chooser's largest city. Chooser doesn't have many ways to respond to this; any normal attempt at retaliation would just launch a larger war that would go very badly for Chooser's country.

Fortunately, Chooser's military has just developed a doomsday device. If launched, the doomsday device will kill everyone in Enemy's country. And Enemy is smart enough, or at least self-interested enough, to not do anything that will lead to the doomsday device being launched. Probably. Unfortunately, the doomsday device will not just kill everyone in Enemy's country, it will also kill everyone in Chooser's country. Fortunately, Chooser also has the ability to tie an automated launcher to the doomsday device, so it will launch if any nuclear missile hits their major city. And they have the ability to let Enemy know that they have tied an automated launcher to the doomsday device. So they can make a very credible threat to Enemy.

If it is likely enough that Enemy will back down when threatened this way, Chooser should install the automated launcher. And, and this is very important, they should make sure Enemy knows that they have done so. Even if there is some small probability $\varepsilon$ that Enemy will launch anyway, if $\varepsilon$ is small enough, and the probability of having everyone in the largest city killed high enough, it is a risk worth taking.

I'd originally thought of making the doomsday device kill not just everyone in chooser's country and Enemy's country, but everyone in the world. (As was the case for the doomsday device in _Dr. Strangelove_.) But this complicates the decision making in ways I'd rather avoid. For one thing we have to account for the loss of future generations. For another, as Jonathan @Knutzen2022 points out, we have to account for the loss of humanity in general, on top of the loss of all those individual humans. Maybe there is no realistic probability of failure small enough that this could be a reasonable risk. But there surely is a probability of failure small enough that the risk of losing a whole country is worth trading off against the certainty of losing the largest city. And that's the risk I'm asking chooser to take in this particular example. And I think in the right circumstances, it's a risk to take.

But now change the example in a few ways. Chooser still has the doomsday device, but they don't have the automated launcher. Fortunately, Enemy is now blessed, or cursed, with a Demon, who can predict with very high probability how Chooser will react if the largest city is destroyed. In particular, the Demon can predict, with very high probability, whether Chooser will react by launching the doomsday device, killing everyone in both countries. Unfortunately, the Demon seems to have predicted that Chooser will not do that, because the nuclear missile is now headed towards the largest city. What should Chooser do?

I think it's very plausible that Chooser should not respond by launching the doomsday device. Even if Chooser wants to punish Enemy country for launching the nuclear missile, which is a reasonable enough wish, the punishment would not be proportionate, and the damage to Chooser's own citizens would be intolerable. If Chooser's only options are the doomsday device or nothing, Chooser has to do nothing. Or so I think; I'll just note that this is an appeal to intuition about a case and that some people may feel differently. But let's explore what happens if you agree that it would be wrong to kill everyone in two countries to try and prevent a nuclear missile launch that's already happened.

So rational choice is not the same thing as the choice a well designed machine would make. And, conversely, a well designed machine will not do just what the rational choice is. We just said that the right way to program the machine, if it is available, is to launch the doomsday device as soon as the nuclear missile is detected. But Chooser, if they are rational, will not launch the doomsday device in response to the nuclear missile. This is a counterexample to Functional Decision Theory (FDT), which says that the rational choice in a situation is the manifestation in that situation of the optimal algorithm [@LevinsteinSoares2020]. The optimal algorithm is the one the machine runs: automatically launch the doomsday device. But that's not what is rational.

The problem here is that launching the doomsday device is what game theorists call a non-credible threat. And you can't make a non-credible threat credible by loudly insisting that you'll really really do it this time, or even that it would be the rational thing to do.

There is another problem for FDT concerning pairs of cases. Change the second example so that Enemy's Demon is actually not very reliable. In this variant, they are better than chance, but not a lot better. Now Chooser certainly wouldn't set up the doomsday machine to automatically launch; the risk of a false positive is too high. So FDT says that in the variant where chooser has to decide what to do after learning the launch was made, chooser will do nothing. And while this is the right thing to do, it is made for the wrong reasons. Once Enemy has launched the missile, Chooser's best estimate yesterday of how reliable the demon is becomes irrelevant to what chooser should do. But according to FDT it could be decisive.

So if decision theory is relevant to building machines that make decisions, it's not because the right decision theory should be built into those machines. And hence it's not because decision theory must be proceduralist in order to make it possible to build it into these machines. It's rather because the people who make the machines face a very hard decision problem about what kinds of machines to build, and decision theory could be relevant to that problem. But how is decision theory relevant to that problem, or indeed any problem? The next section looks at that question in more detail.

## Why Do Decision Theory {#why}

What are we trying to do when we produce a decision theory? I think some of the disputes within the field come from different theorists having different motivations, and hence different answers to this question. My answer is going to be that decision theory plays a key role in a certain kind of explanatory project. And I think defensivism is well suited to play that role. But to see what I mean by playing a key role in an explanatory project, it helps to compare that with other possible views about the aim of decision theory.

One thing you might hope decision theory would do, and certainly one thing students often expect it will do, is provide advice on how to make decisions. I think decision theory is very ill-suited to this task, and it shouldn't really be the aim of the theory. The primary reason for this is that in any real life situation, the inputs are too hard to identify. To use decision theory as a guide to action, I need to know the utility of the possible states. And I need to know not just what's better and worse, but how much they are better or worse. At least speaking for myself, the only way I can tell the magnitudes of the differences in utilities between states is to ask about various gambles, and think about which of them I'd be indifferent between. That's to say, the only way I can tell that the utility of $A$ is half way between that of $B$ and $C$ is to ask whether I'd be indifferent between $A$ and a 50/50 chance of getting $B$ or $C$. So I have to know what decisions I'd (rationally) make before I can work out the utilities. And that means I have to know what decisions to make before I can even apply decision theory, which is inconsistent with thinking that decision theory should be the guide to what decisions to make. This isn't such a pressing problem when decisions can be made using purely ordinal utilities, but those cases are rare. So in general there is little use for decision theory in advising decisions.

A somewhat better use for decision theory is in evaluation. Using decision theory, we can look at someone else's actions and ask whether they were rational. This is particularly pressing in cases where the person has harmed another, perhaps due to possible carelessness, or in defense of another, and we're interested in whether their actions were rational. Now one immediate complication in these cases is that we don't know the actor's value function. Even if the action doesn't maximise value as we see it, we don't know whether they have a different value function (perhaps one that puts low weight on harms to others), or they were doing a bad job maximising value. We don't know whether they were a knave or a fool. But it's often charitable to assume that they do have a decent enough value function, and we can ask whether what they were doing was rational if they did indeed have a decent value function.

This is a task decision theory is useful for, but it alone wouldn't justify the existence of books like this one. For one thing, the theory of how to act around Demons isn't usually relevant to whether an act was careless, or a permissible kind of self/other-defense. It is sometimes relevant. Sometimes we should think game-theoretically about whether a person was acting properly, and that will bring up issues that are similar to issues involved in making decisions around Demons. But usually the kind of decision theory we need in these cases is fairly elementary. 

A bigger problem is that rationality is too high a bar in these cases. (I'm indebted here to Jonathan Sarnoff.) Imagine that $a$ puts up a ladder somewhat sloppily, and it falls and injures $b$. The question at hand is whether $a$ is morally responsible for the injury to $b$ due to their carelessness. Ladders are tricky things, and sometimes one can take reasonable precautions and bad things happen anyway. Sometimes an injury is correctly attributed to bad luck even if a super-cautious person would have avoided it. We aren't in general obliged to take every possible precaution to avoid injuring others. (If we were, we wouldn't be able to go out in public.) So what's the test we should use for whether this particular injury was just a case of bad luck or a case of carelessness? It is tempting to use decision theory here. The injury is a case of bad luck iff it was decision-theoretically rational for $a$ to act as they did, assuming they had a decent value function. The problem is that this is a really high bar. Imagine that $a$ did what any normal person would do in setting up the ladder, but there was a clever way to secure it for minimal cost that $a$ didn't notice, and most people wouldn't have noticed. Then there is a good sense in which what $a$ did was not decision-theoretically rational; the value maximising thing to do was the clever trick. But we don't want people to be morally responsible every time they fail to notice a clever option that only a handful of people would ever spot. And this is the general case. Decision-theoretic rationality is a maximising notion, and as such it's a kind of hard norm to satisfy. We don't want every failure to satisfy it in cases of unintentional injury to others, or intentional injury to others in the pursuit of a justifiable end like self-defence, to incur moral liability. So this isn't actually a place where decision theory is useful.

And if decision theory isn't useful in these cases, then it's value as an evaluative tool is somewhat limited. We can still use it for going around judging people, and saying that was rational, that was irrational. And that's a fine pastime, being judgmental can be fun, especially if the people being judged are in charge of institutions we care about. But we might hope for a little more out of our theory.

A third role for decision theory is in predicting what people will do. Sometimes we know people's incentives well enough to be able to predict that they will act as if they are rational. And at least sometimes, that can lead to surprising results. I'll talk through one case that I find surprising, and I suspect other philosophers will find surprising too.

Chooser runs a televised rock-paper-scissors tournament. Ratings are fine, but Chooser is told by the bosses that what the audience really likes is when rock beats scissors. The audience doesn't think rock-paper-scissors is really violent enough, and the implicit violence of rock smashing scissors is a help. So Chooser is thinking about generating more outcomes where rock beats scissors. And their plan is to make a cash payoff to players every time they successfully play rock, on top of the point they get for winning the game. Chooser's hope is that the game payoff will change from the standard payoff table, which looks like this

```{r, simple-rps, cache=TRUE}
rps <- tribble(
	   ~"",         ~Rock,   ~Paper, ~Scissors,
	   "Rock",      "0, 0", "-1, 1",  "1, -1",
	   "Paper",    "1, -1",  "0, 0",  "-1, 1",
	   "Scissors", "-1, 1", "1, -1",   "0, 0"
	)
gameformat(rps, "Rock-Paper-Scissors")
```

to this

```{r, adjusted-rps, cache=TRUE}
rps_adjust <- tribble(
	   ~"",         ~Rock,   ~Paper, ~Scissors,
	   "Rock",      "0, 0", "-1, 1",  "2, -1",
	   "Paper",    "1, -1",  "0, 0",  "-1, 1",
	   "Scissors", "-1, 2", "1, -1",   "0, 0"
	)
gameformat(rps_adjust, "Rock-Paper-Scissors with Bonus for Rock")
```

And they turn to their resident decision theorist to ask how much this will improve ratings. If you haven't worked through this kind of problem before, it's actually a fun little exercise to work out what the effect of this change will be. Because the disappointing news they are going to get from the decision theorist is that this move will backfire. After the change the rock-scissors combination will occur less often than it did before the change.

In the original game, it's pretty clear what the unique equilibrium of the game is. Each player plays each option with probability $\frac{1}{3}$. If either player had any deviation from that, then they would in the long run be exploitable. So that's what they will do, over a long enough run. And that means that a combination where one player chooses Rock and the other chooses Paper will occur in $\frac{2}{9}$ of games.

But what's the equilibrium of the new game? It's symmetric, each player uses the same mixed strategy. And in that mixed strategy, a player chooses Paper with probability $\frac{5}{12}$, Rock with probability $\frac{1}{3}$, and Scissors with probability $\frac{1}{4}$. So the combination where one player chooses Rock and the other chooses Paper will occur in $\frac{1}{6}$ of games, a considerable reduction from what we previously had.

It is very easy to share the intuition that if you reward a certain kind of behavior, you'll see more of it. But that doesn't always work in the context of competitive games. Here rewarding Rock doesn't result in any change to how often Rock is played, but does result in a reduction of how often one plays the strategy that Rock defeats. What it actually incentivises is behavior that is outside this Rock-Scissors interaction, i.e., Paper. Now this doesn't require a huge amount of decision theory to work out - it's pretty simple linear algebra. But the intuition that rewarding a kind of behavior causes it to be more common is widespread enough that I think a theory that predicts it won't happen isn't completely trivial.

So cases like these are cases where, I think, decision theory has a useful predictive function. And this means it has practical advantages to the institutional designer, i.e., the chooser in this case. Knowing a bit of decision theory will tell them not to literally waste their money on this plan to reward players who win playing rock. Does it also have practical advantages to the players? Perhaps it has some, though it's a little less clear. After all, if every other player finds the new equilibrium quickly enough, then the expected return of each strategy in a given game will be equal. Decision theory itself says that in a particular play it doesn't matter what a player does. So it kind of undermines its own claim to being practically significant to the players. But this shouldn't reduce how useful the theory is at predicting how players will react to changes in the institutional design, and hence how valuable the theory could be to institutional designers.

But while decision theory can be predictively useful, the main role it plays is in explaining human behavior. Think about the explanation George Akerlof offers of why used cars lose value so quickly, i.e., why people don't pay nearly as much for lightly used cars as they play for new cars [@Akerlof1970]. Or about the explanation Michael Spence offers for why employers might pay more to hire college graduates even if college does not make employees more productive [@Spence1973]. In each case carefully thinking about the decision problem each actor faces can give us a story about how behavior that looks surprising at first actually makes sense.

The point is not that these explanations always work. Both of them make substantive assumptions about the kind of situation that actors are in. I'm inclined to think that the assumptions in Akerlof's model are close enough to true that his explanation works, and the ones in Spence's model are not. But whether you think that's true or not, what you should think is that models like these show how decision theory can play a role in simple but striking explanations of otherwise mysterious behaviour.

A key assumption in each such explanation is that actors are basically rational. Or, at least, that their behaviour is close enough to what it would be if they were rational that rationality is a good enough assumption for explanatory purposes. A long tradition in philosophy of economics is that this is a fatal weakness in these explanations. After all, we know that people are not in fact perfectly rational. But I'm inclined to think it is a strength, in fact an important strength of decision theoretic explanations of behavior.

The fact that people are not perfectly rational does not mean that we cannot explain their behaviour using models that assume rationality. All that we need for that is that in a particular situation, the behaviour is as it would be if they were rational. And that can be true in certain domains. For example, people who prefer vanilla ice cream to strawberry ice cream buy more vanilla ice cream than strawberry ice cream. Now wheeling out a belief-desire model of action, combined with an assumption that ice-cream purchases are made by practically rational actors, to explain that pattern of purchases would be overkill. But it wouldn't be wrong. In some cases people collectively do act as they would if they were all rational. Not all cases, of course, but some. In each case, we have to look.

To know whether people are acting rationally, we sometimes need to have a sophisticated theory of decision. At first glance, it might look irrational to have a very strong preference for new cars over lightly used cars. It takes some work to see that it might be, as Akerlof argued, a rational reaction to epistemic asymmetries. This work is a project that decision theory can contribute to, and indeed has contributed to.

The project of at least trying to see how surprising behaviour might be rational, a project which decision theory has a key role in, is valuable for two reasons. 

One reason is epistemic. It's really easy to fall into thinking that certain behaviour is the result of a bias, and not even look for possible rational explanations of it. This is what Brighton and Gigerenzer call the 'bias bias' [@BrightonGigerenzer2015]. You don't even have to posit a bias in favor of explanations in terms of irrationality to get this result. Often the explanation in terms of irrationality is easier. It's much easier to say that people have an irrational attachment to new cars than to build a model of rational choice under epistemic asymmetry that explains the behaviour. And obviously its easier to settle for easy explanations. A commitment to looking for rational explanations of behaviour is a useful practice because it makes the researcher not settle for simple explanations. It might be that on a particular occasion the simple explanation is right, and people are just being irrational. But it is often a good use of time to at least look at what the best rational explanation is, and see if it is as plausible as the best irrational explanation.

Another reason is moral. There is a kind of respect involved in treating people as rational, or at least taking as a live option that people are acting rationally unless there is fairly strong reason to believe they are not. And we should show this kind of respect to other humans. And explanations of behaviour in terms of rational choice typically have the advantage that they make sense not just to the theorist, but to the person actually carrying out the behaviour. They allow for at least the possibility of the theorist and the person being theorised about to understand the behaviour in the same way. And that's a kind of equality that we should value.

The upshot of these considerations is that we should try to see how surprising behaviour could be rational. Rather than seeing someone as incompetently trying to carry out our ends, we should consider the possibility that behaviour we find surprising is the result of differences in what evidence is available, or in what values the actors have. Making sure we at least check what an explanation in terms of rational choice would look like is a useful heuristic because it sometimes turns up surprising and plausible models. It's an empirical question whether it is an efficient heuristic. I suspect it is, but maybe there are other heuristics that more efficiently lead to plausible models. Even if that were so, I would still think that it would be good to start by looking for rational choice models. That's because the moral reasons in favor of looking for these kinds of models, or for these kinds of explanations, would be decisive.

That's what I think the primary purpose of decision theory is. It's part of the project of trying to explain surprising aspects of human behaviour in terms of rational choices by people with different amounts of evidence, and different values. And since that's a very valuable project, I think decision theory is valuable, at least insofar as it contributes to the project.

For what it's worth, I think the kind of decision theory I'm defending, where the central principle is that choices must be defensible, has a much better track record of contributing to rational explanations of surprising behaviour than do its rivals. I don't know of real world situations in which we see Evidential Decision Theory play a particularly useful role in explaining what people do. Are there any papers based on EDT that as good as Akerlof's original paper on the market for lemons? Even if there are no such papers, it is possible that there are institutional reasons for this. Maybe not enough economists or political scientists get taught EDT, or maybe malicious journal editors conspire to not publish papers using models based on EDT. But if the role of decision theory is, as I've argued, to contribute to these kinds of explanations, it would be useful to see how much of a contribution to rival philosophical theories of decision could actually make.



## Why Do Ideal Decision Theory {#ideal}

The previous section was on why philosophers should care about decision theory. But what we're doing here isn't just decision theory, it's a very specific kind of decision theory. What we're doing might be called ideal decision theory. It's the theory of how idealised agents make decisions. And I'm going to appeal to those idealisations a fair bit in what follows. This section is about why we should care about such an idealised theory, and in particular about why a defensivist decision theory should care about it.

I'm hardly alone in focussing on the ideal case. Every theory of decision in the philosophical literature does the same thing. This is surprising because focussing on the ideal case is even less intuitive for the proceduralist than the defensivist, and most decision theorists are proceduralists. But they do indeed focus on the ideal case. We can see that they do by thinking about how they handle cases involving bets on mathematical propositions. Since these will play a bit of a role in what follows, particularly in chapter \@ref(dilemma), I want to set up a particular bet carefully.

Say that a positive integer is **speven** iff the second digit of its largest prime factor is even. By 'second digit' here, I mean second when reading from left to right. So the second digit of 5743 is 7, and that (prime) number is not speven. But the second digit of 5843 is 8, and that (prime) number is speven. Calculating whether a number is speven gets very hard once the number has more than a few dozen digits. But whether it is speven is necessary and a priori, and invariant across all models for arithmetic. So on any standard conception of probability, the probability that a number is speven is 1 or 0. Still, we don't always act like it is speven.

Let $n$ be some large number, say one whose decimal representation has over 100 digits. And say Chooser is offered a choice between saying "Yes" (meaning $n$ is speven), "No" (meaning it isn't) or "Pass", with the payouts in dollars for each choice given by this table.

```{r, basic-speven, cache=TRUE}
basic_speven <- tribble(
	   ~"",     ~`Is Speven`, ~`Is Not Speven`,
	   "Yes",    1, 0,           
	   "Pass",   0.8, 0.8,
	   "No",     0, 1
	)
gameformat(basic_speven, "A Basic Example of Betting on Arithmetic")
```

Any decision theory in the philosophical literature will say that Pass is the wrong option here. The probability that $n$ is speven is either 1 or 0, so either Yes or No maximises expected utility. But intuitively, the right thing to do is to Pass. The alternative is either figuring out whether $n$ is speven, and it isn't worth spending that energy for 20 cents, or guessing, which will on average do worse than playing Pass.

The lesson I take from this example is that every theory of idealised decision making needs to be complemented with a theory of non-ideal decision making. And that latter theory should say that Pass is the right choice in this game. But this isn't the only lesson one could take from the example. Another lesson could be that ideal decision theory is a pointless enterprise. It should not be supplemented by non-ideal decision theory, but replaced. I don't think that's right, but it takes some argument to say why it isn't right. That longer argument will come in chapter \@ref(dilemma), but for now I'll just say what positive role I think ideal decision theory has to play.

Let's start with some things that idea theory cannot do. It can't give people a target they should approximate. That's because the following is a very bad argument.

1. The ideal is X.
2. So, Chooser should be as much like X as they can be.

We know that isn't right for reasons set out by @LipseyLancaster1956. If one can't be like the ideal, it is often best to do other things that the ideal chooser would not do to offset these failings. Here's one simple example. The ideal chooser, in decision theory, can do all reasoning instantaneously. So it's a bad idea for them to stop and have a think about it before making a big decision. Since they have thought all the thoughts that are needed, that would just be a waste of time. But it's often a very good idea for Chooser to stop and have a think about it before making a big decision. Not doing that, in order to be more like the ideal, is a mistake.

The following argument isn't as bad, but it isn't right either.

1. The ideal is X.
2. Chooser's situation is approximately ideal.
3. So Chooser should do approximately X.

The situations where this fails are a bit more contrived than the situations where the previous argument failed, at least for typical individuals. But here the details of Lipsey and Lancaster's argument matter. At least when Chooser is designing institutions, like market structures or taxation systems, it turns out to very often be the case that the the second best solution looks dramatically different to the best solution. And someone who is approximately ideal might only be able to find the second best solution, not the best one, in a reasonable time. So it's possible that someone with very mild computational limitations to do something very different from what the ideal agent would do, and yet be acting optimally given their limitations.

The following two arguments, however, are good arguments. And the two main uses of ideal decision theory are related to these two good arguments.

1. The ideal is X.
2. The differences between Chooser's situation and the ideal are irrelevant.
3. So Chooser should do X.

Ideal theory can provide advice, in situations that are like the ideal in suitable ways. It isn't trivial for the ideal theory to provide such advice though. The second premise is often very hard to justify. But in some cases it is not that hard - the computations that have to be made are easy, and the stakes are high, so it is worth spending the resources to make all the computations. And in those cases, we expect Chooser to act like the ideal. "Expect" here has both a normative and a descriptive meaning, and let's make the latter of those explicit with another good argument that uses ideal decision theory.

1. The ideal is X.
2. The differences between Chooser's situation and the ideal are irrelevant.
3. So, Chooser will do X.

If we interpret 'situation' in premise 2 to include the fact that Chooser is (approximately) rational, then this is a good argument too. And when we have an argument like this, we can use it to predict what Chooser will do, and explain what Chooser has done. 

This kind of argument can be used in explanations that have the structure Michael @Strevens2008 argues that explanations involving idealisations always have. They include a model saying what would happen in idealised situations. And they include a premise that doesn't just say that the real situation is close to the ideal, but that the differences don't matter for the purposes of what we're trying to explain. That kind of structure covers simple explanations using the ideal gas law you might learn in introductory chemistry, and it also covers explanations using ideally rational agents. What we need for, e.g., Akerlof's explanation of used car prices to work is not that everyone in that market is perfectly rational, but that they are close enough to rational for the purposes of predicting how they will behave in the used car market. That's important because participants in the used car market are not perfectly rational; they might not even be close to it. But just as real molecules can be modeled by things that are infinitely smaller than them, real buyers and sellers of used cars can be modeled by actors that are infinitely more rational than they are. 

And that's the big picture project that I want this book to be contributing to. There are both epistemic and moral reasons to look for explanations of behaviour in terms of individuals acting rationally. These explanations will be idealised explanations. Idealised explanations involve describing carefully how things work in the ideal, and arguing that the differences between the ideal and the reality are unimportant for the particular thing being explained. The latter task is hard, and often not done with sufficient care, but isn't impossible. This book contributes primarily to the former task, though especially in chapter \@ref(dilemma) I'll have things to say relevant to the latter task as well.

David Lewis gives a similar account of the purpose of decision theory in a letter to Hugh Mellor. The context of the letter, like the context of this section, is a discussion of why idealisations are useful in decision theory. Lewis writes,

> We’re describing (one aspect of) what an ideally rational agent would do, and remarking that somehow we manage to approximate this, and perhaps – I’d play this down – advising people to approximate it a bit better if they can. [@Lewis1981Mellor, 432]

To conclude this section on an historical note, I want to compare the view I'm adopting to the position Frank Knight puts forward in this famous footnote.

> It is evident that the rational thing to do is to be irrational, where deliberation and estimation cost more than they are worth. That this is very often true, and that men still oftener (perhaps) behave as if it were, does not vitiate economic reasoning to the extent that might be supposed. For these irrationalities (whether rational or irrational!) tend to offset each other. The applicability of the general "theory" of conduct to a particular individual in a particular case is likely to give results bordering on the grotesque, but _en masse_ and in the long run it is not so. The _market_ behaves _as if_ men were wont to calculate with the utmost precision in making their choices. We live largely, of necessity, by rule and blindly; but the results approximate rationality fairly well on an average. [@Knight1921 67n1]

Like Knight, I think that explanations in social science can treat people as rational even if they are not, even if it would "given results bordering on the grotesque" to imagine them as perfectly rational. And that's because, at least in the right circumstances, the irrationalities are irrelevant, or they cancel out, and the "as if" explanation goes through. Now I do disagree with the somewhat blithe attitude Knight takes towards the possibility that these imperfections will not cancel out, that they will in fact reinforce each other and be of central importance in explaining various phenomena. But that's something to be worked out on a case-by-case basis, not presupposed in advance that the imperfections will either be explanatorily irrelevant or decisive.

There is one other point of agreement with Knight that I want to emphasise. If we don't act by first drawing Marshallian curves and solving optimisation problems, how do we act? As he says, we typically act "by rule". Our lives are governed, on day-by-day, minute-by-minute basis, by a series of rules we have internalised for how to act in various situations. The rules will typically have some kind of hierarchical structure - do this in this situation unless a particular exception arises, in which case do this other thing, unless of course a further exception arises, in which case, and so on. And the benefit of adopting rules with this structure is that they, typically, produce the best trade off between results and cognitive effort. 

This isn't finished

## Why Not Proceduralism {#whynot}

Let's take stock. This chapter has been a response to the following two kinds of worries.

- The best thing that decision theory could do would be to provide a procedure for making good decisions.
- If decision theory can't do that, it's a pointless activity.

So far the attention has been primarily on the second point. I've argued that it isn't true - that decision theory can have an important role in explanations of social phenomena even if it doesn't provide a procedure for making good decisions. But what about the first point? Even if this is a role for decision theory, wouldn't a procedure for good decisions be better? In some sense perhaps it would be, but there is no reason to think that anything like decision theory is going to be part of such a procedure for creatures like us.

To do

- Discuss the big world not being a small world - cite Binmore 2017
- Discuss the computational problems
- Discuss the violations of sure thing in multiple speven games
- Say what's really good - sorting the cliches into good and bad ones, developing better cliches etc. And defensivism is just as useful for that, maybe better, than proceduralism

# Against Decisiveness {#decisive}

# Against Rational Possibility {#dilemma}

## The Possibility Assumption in Philosophical Arguments {#badargument}

The bad argument against utility theory - pick a number in [0, 1), every positive is ruled out so 0 huh?!


## A Recipe for Counterexamples {#recipe}

Once you see the idea behind the argument in section \@ref(badargument), it's easy to see how to construct 'counterexamples' to any decision theory that allows for dilemmas. For instance, here is a general purpose recipe for constructing counterexamples to most forms of Causal Decision Theory, causal defensivism included.

Step One 
:    Find some game where one player has demon-like payouts, and there is no pure strategy equilibrium for the other player. By a 'demon-like payout', I mean that player's payouts are all 1s and 0s, and you can sensibly interpret the 1s as situations where they correctly predicted the other player's choice.

Step Two 
:    Translate that game into a demonic decision problem, taking care to stipulate that the human player is incapable of playing mixed strategies, or that something very bad will happen to them if they do.^[I'm a bit perplexed as to why this stipulation is so widely accepted in the decision theory literature. But it usually seems to be accepted with minimal fuss. We'll return to this point in section \@ref(mixedavoidance).]

Step Three
:    Look at the pure strategies the player has, and identify the one that it would be least plausible to have one's theory recommend.

Step Four 
:    Argue, correctly, that all strategies other than the one you've identified are irrational according to CDT.

Step Five 
:    Infer that the remaining strategy is the one that CDT recommends.

Step Six
:    Point out that it is really unintuitive that CDT would recommend that strategy, declare that this is a clear counterexample to CDT, declare victory, etc.

Hopefully the discussion in section @\ref(badargument) will have made it clear that we can run this recipe against just about any theory, so it overgenerates. And not just that, we can identify the misstep - it's step five. 

## Betting Against the Demon {#againstdemon}

There is a slight variation on the recipe in an interesting example due to Arif @Ahmed2014 [sect 7.4.3]. Here the plan at step 6 is not to argue that CDT gets the wrong result, in fact Ahmed endorses the conclusion he thinks CDT ends up with, but to argue that CDT undermines its own principles. I'm going to lean a bit on the discussion of the example in the review of Ahmed's book by James @Joyce2016, though I end up drawing a slightly different conclusion to Joyce about the argument. And I'm going to start with a simplified version of the example, where I think it's clearer where things go wrong, and build up to the version Ahmed gives. (I've also relabeled the moves, because I find the labels here more intuitive.)

The simple version of Ahmed's example, which we'll call Betting Against the Demon, has three stages.

Stage One
:    Demon predicts whether Chooser will choose 1 box or 2 boxes at stage two.

Stage Two
:    Chooser chooses 1 box or 2 boxes. They receive $100 if Demon predicted they will choose 1 box, and nothing if Demon predicted they will choose 2 boxes, whatever they choose. This payout is not revealed to them until the end.

Stage Three
:    Chooser selects one of two bets. Bet R (for right) wins $25 if Demon predicted correctly, and loses $75 if Demon predicted incorrectly. Bet W (for wrong) wins $75 if Demon predicted incorrectly, and loses $25 if Demon predicted correctly.

After this the moves are revealed, and Chooser gets their rewards. Here is the strategic form of the game, assuming Demon wants to make correct predictions, and ignoring strategies that differ only in what Chooser does in worlds that are ruled out by their Stage Two choice. (I'll leave it as an exercise for the reader to confirm that they aren't relevant to the analysis.)

```{r, betting-against-demon, cache=TRUE}
demon_betting_strategic <- tribble(
	   ~"", ~P1, ~P2,
	   "1R", "125,1", "-75,0",
	   "1W", "75,1", "75,0",
	   "2R", "25,0", "25,1",
	   "2W", "175,0", "-25,1"
	)
gameformat(demon_betting_strategic, "The strategic form of Betting Against the Demon")
```

For Chooser, each row sets out what they do at stage two, and what they do at stage three - the number is whether they pick 1 box or 2, and the letter is whether they take bet R or W. For Demon, P1 is that they predict 1 box, and P2 is that they predict 2 boxes.

It should be clear enough that there are no pure strategy Nash equilibria for this game. The best response to P1 is 2W, but the pair of 2W and P1 is not a Nash equilibrium. And the best response to P2 is 1W, but the pair of 1W and P2 is not a Nash equilibrium. 

But of course this game is not a strategic form game, it's an extensive form game. Here is the game tree for it.

```{tikz, betting-against-demon-tree, fig.cap = "The game tree for the betting against the demon example", fig.ext = 'png', cache=TRUE}
\usetikzlibrary{calc}
\begin{tikzpicture}[scale=1.4,font=\footnotesize]
\tikzset{
% Two node styles for game trees: solid and hollow
solid node/.style={circle,draw,inner sep=1.5,fill=black},
hollow node/.style={circle,draw,inner sep=1.5},
square node/.style={rectangle,draw, inner sep = 1, fill = black}
}

% Specify spacing for each level of the tree
\tikzstyle{level 1}=[level distance=22mm,sibling distance=25mm]
\tikzstyle{level 2}=[level distance=15mm,sibling distance=15mm]
\tikzstyle{level 3}=[level distance=17mm,sibling distance=10mm]
% The Tree
\node(0)[hollow node,label=below:{$Demon$}]{}
child[grow=left]{node(5)[solid node]{}
child[grow=up]{node(1)[solid node]{}
child{node[square node,label=above:{$125,1$}]{} edge from parent node [right]{$R$}}
child{node[square node,label=above:{$75,1$}]{} edge from parent node [left]{$W$}}
edge from parent node [left]{$1$}
}
child[grow=down]{node(3)[solid node]{}
child{node[square node,label=below:{$175,0$}]{} edge from parent node [left]{$W$}}
child{node[square node,label=below:{$25,0$}]{} edge from parent node [right]{$R$}}
edge from parent node [left]{$2$}
}
edge from parent node [below]{$P1$}
}
child[grow=right]{node(6)[solid node]{}
child[grow=up]{node(2)[solid node]{}
child{node[square node,label=above:{$-75,0$}]{} edge from parent node [right]{$R$}}
child{node[square node,label=above:{$75,0$}]{} edge from parent node [left]{$W$}}
edge from parent node [right]{$1$}
}
child[grow=down]{node(4)[solid node]{}
child{node[square node,label=below:{$-25,1$}]{} edge from parent node [left]{$W$}}
child{node[square node,label=below:{$25,1$}]{} edge from parent node [right]{$R$}}
edge from parent node [right]{$2$}
}
edge from parent node [below]{$P2$}
};

% information set
\draw[dashed](1) to (2);
\draw[dashed](3) to (4);
\draw[dashed, bend left](5) to (6);
% specify mover at 2nd information set
\node at ($(1)!.5!(2)$) [above]{$Chooser$};
\node at ($(3)!.5!(4)$) [below]{$Chooser$};
\node at ($(5)!.5!(6)$) [above, yshift = 25] {$Chooser$};
\end{tikzpicture}
```

Demon moves first, but Chooser is not alerted to Demon's move until the end of the game. So at every stage Chooser has two nodes in their information set - one for each of Demon's possible moves. But Chooser does know their own prior move, so at stage three the information sets do not include both the top and bottom of the tree. This looks a lot like a signaling game, but there are some notable differences. There is no move from Nature here; rather Demon moves once and Chooser moves twice. And there is an information set connecting the nodes in the middle-left and middle-right of the tree.

Since it is a game where Chooser moves twice, the relevant game theoretic concept to use is Bayesian Perfect Equilibrium. A strategy is defensible for Chooser iff it is part of a Bayesian Perfect Equilibrium. Since all Bayesian Perfect Equilibria are Nash Equilibria, and the game has no Nash Equilibria, there are no defensible moves. But the fact that it is a dynamic game is important to Ahmed's argument.^[I'm only going to discuss what Ahmed says about CDT combined with so-called 'sophisticated' choice, since it's the right way to make sequences of decisions.]

Assuming that Chooser plays a pure strategy, in equilibrium Demon knows that strategy, and Chooser knows this. So at stage three, Chooser will believe that choosing R will win $25 more or less for sure, and choosing W will lose $25 more or less for sure. So Ahmed infers, correctly, that CDT says that Chooser should not choose W. From this he infers, incorrectly, that CDT says that Chooser should choose R.^[Strictly speaking, what he shows is that this follows given that we are using the version of CDT described by @Lewis1981b. But he says earlier, in section 2.8 of his book, that he thinks all versions of CDT will agree with Lewis's version about everything that is covered in the book. As he puts it, "As far as this discussion is concerned, any one of them could stand for their disjunction." This is false, and importantly so at just this point. The version of CDT developed by @Skyrms1990 for instance would not agree with Lewis here, and would not say that any pure strategy is rational. Ahmed does discuss some of Skyrms's earlier work, but not the version of CDT that is developed from 1990 onwards using equilibrium concepts.] This doesn't follow without an extra, incorrect, assumption that CDT says that something should be chosen in this case. But in fact, since the game has no pure strategy equilibria, that isn't true. Anyway, from that false assumption, Ahmed infers, correctly, that CDT recommends playing some strictly dominated strategy or other. And that's incoherent, since CDT is motivated by the thought that one should never play strictly dominated strategies.

I've simplified matters a little here, but not in a way that matters. Ahmed doesn't say that Demon is arbitrarily accurate, but only that Demon is 80% accurate. But it's easy enough to modify the game to accommodate this. In fact there are two ways to do so, following the two tricks I introduced in section \ref(familiar). The Smullyan approach is to have a stage 0, where the Demon is assigned to one of two types: Knight or Knave. The former has an 80% chance of being the assignment. The Demon is told the assignment but Chooser is not. And if Demon is assigned Knave, their payouts are flipped - they get 1 if they play P1 and Chooser selects 2, or they play P2 and Chooser selects 1. The Selten approach is to have a stage 1A, between Demon and Chooser's moves. Demon chooses P1 or P2, then at stage 1A, there is a 20% chance that Nature will reverse Demon's choice, and the payouts will be a function not of what Demon did, but what happened after Nature modified Demon's choice. The trees for either case are impossibly messy, and I won't draw them. But it doesn't matter - the equilibrium analysis of either game is exactly the same as the equilibrium analysis of the game where Demon is arbitrarily accurate, and I'll stick with that game from now on.

So does CDT recommend playing a dominated strategy? Of course not. As Joyce says, CDT is "built to guarantee" that it doesn't recommend dominated strategies [@Joyce2016, 231]. What it says, or at least what causal defensivism says, is two-fold.

If Chooser can randomise, they should randomise. They should play the one and only equilibrium strategy in this game, which is a 50/50 mixture of 1W and 2W. In equilibrium, Demon will play a 50/50 mixture of P1 and P2. So at stage 3, Chooser will think it is 50% likely that Demon has 'predicted' correctly. (The scare quotes are because calling the output of Demon's mixed strategy a prediction stretches language just about to breaking point.) So Chooser will be happy sticking with their strategy at stage 3 - it will have an expected return of $25, and the alternative has an expected return of -$25.

If Chooser can't randomise, then it's a dilemma. What we say here should be similar to what EDT, or any other decision theory, says about the example in section \@ref(recipe). But what is that? Let's come back to that question in a bit - first I'll address some objections to the very idea that there could be dilemmas in decision theory.

## Mixtures and Dilemmas {#mixedavoidance}

Various theorems exist proving the existence of equilibria in games. (See @BonannoText

## What Dilemmas are Like

Be like a smart rationally constrained person

Use heuristics that work much of the time, on average

This might end up looking a lot like EDT
Maybe this is why Newcomb's Problem is so hard

# Against Coherence Norms {#coherence}

Some say decision theory is just about doing well by one's own lights.

I follow Comesana 2020 in saying rational action requires rational belief.

Actually doesn't quite require - Knight's kid playing in the field is fine

And I mean I think something stronger than Comesana - I mean that there isn't anything extra good re coherence

## Coherence for the Incoherent

What is it to do best by one's own lights

If one believes p, q -\> -p, and q, is it best to act as if p, or as if q

At best we can get rules for how to be more coherent if you are a bit coherent

And that's maybe something, but not a lot

But even this requires there being a gap between coherence norms and evidence norms, and I don't think there is

## Coherence is a Substantive Norm

Summarise Worsnip book

Intuition about guy who punches self in head because he thinks it will mean some trivial thing he cares about is realised

Intuition about guy who goes from p to p v  q1, p v q2, etc, etc, all day every day

Intuition that Dummett/Priest etc are, if wrong about anything, wrong about a substantive matter

Worsnip response - everyone has a tendency to be coherent

Response - what, and I cannot stress this enough, the f

There is no way that's a correct description of the dialethist, the intuitionist, etc

## Coherence in Signaling Games

So far I've offered the following argument against the view that decision theory is only about how people should act given their existing beliefs and desires, and has no interest in the rationality of the beliefs.

1. The view does not make sense when applied to people whose beliefs are not just irrational, but incoherent.
2. So the view needs a distinction between coherence norms on belief, which must be satisfied for decision theory to be applicable, and substantive norms on belief, which are  irrelevant to decision theory.
3. But thinking about heterodox logicians reveals that there is no distinction to be found here.
4. So, the view does not ultimately make sense.

Here I want to change tack and offer a direct argument, from with decision theory, for the argument that the decision-theoretic notion of rational action is sensitive to the rationality of the chooser's underlying beliefs. The argument is going to be that the best solution to the beer-quiche game [@ChoKreps1987] requires that we look at the rationality of the underlying beliefs, not just at which actions flow in the right way from existing beliefs.

To start, let's translate the beer-quiche game into decision-theoretic terms, using an arbitrarily accurate demon. The problem is a little more complicated than Newcomb-like problems often are, but it should be reasonably familiar if one is used to the kind of signaling games first developed by David Lewis [-@Lewis1969a]. The game goes through the following stages.

1. Both Chooser and Demon are informed of all the following facts, and it is made clear that they are common knowledge.
2. Chooser is randomly assigned to one of two _types_, which we'll call $u$ and $d$, for Up and Down. This assignment is done by a random device which has an 0.6 chance of assigning Chooser to $u$, and an 0.4 chance of assigning Chooser to $d$. Demon is not told of the assignment, and cannot predict how random devices work.
3. Chooser will then make a choice of two options, which we will label $U$ and $D$. Demon will be told which option Chooser takes.
4. Demon will then try to guess which type Chooser is.
5. In making this guess, Demon will use their arbitrarily good ability to predict Chooser's _strategy_. The strategy, in the relevant sense, is Chooser's function from type assignment to choice. Chooser can randomise, so a function is a pair of probabilities - what probability of selecting $U$ if they are type $u$, and what probability of selecting $U$ if they are type $d$.
6. Chooser gets 2 utils if Demon predicts they are type $u$, and 1 util if their choice 'matches' their type, i.e., if they select $U$ if they are $u$ or $D$ if they are $d$.
7. Demon gets 1 util if their guess is correct.

Here is the game in graphical form.

```{tikz, beer-quiche, fig.cap = "A Signaling Game", fig.ext = 'png', cache=TRUE}
\usetikzlibrary{calc}
\begin{tikzpicture}[scale=1.4,font=\footnotesize]
\tikzset{
% Two node styles for game trees: solid and hollow
solid node/.style={circle,draw,inner sep=1.5,fill=black},
hollow node/.style={circle,draw,inner sep=1.5}
}

% Specify spacing for each level of the tree
\tikzstyle{level 1}=[level distance=12mm,sibling distance=25mm]
\tikzstyle{level 2}=[level distance=15mm,sibling distance=15mm]
\tikzstyle{level 3}=[level distance=17mm,sibling distance=10mm]
% The Tree
\node(0)[solid node,label=right:{Nature}]{}
child[grow=up]{node[solid node,label=above:{
$Chooser$
}] {}
child[grow=left]{node(1)[solid node]{}
child{node[hollow node,label=left:{$(3,1)$}]{} edge from parent node [above]{$u$}}
child{node[hollow node,label=left:{$(0,0)$}]{} edge from parent node [below]{$d$}}
edge from parent node [below]{$U$}
}
child[grow=right]{node(3)[solid node]{}
child{node[hollow node,label=right:{$(1,0)$}]{} edge from parent node [below]{$d$}}
child{node[hollow node,label=right:{$(2,1)$}]{} edge from parent node [above]{$u$}}
edge from parent node [below]{$D$}
}
edge from parent node [left]{$0.6$}
}
child[grow=down]{node[solid node,label=below:{
$Chooser$
}] {}
child[grow=left]{node(2)[solid node]{}
child{node[hollow node,label=left:{$(2,0)$}]{} edge from parent node [above]{$u$}}
child{node[hollow node,label=left:{$(0,1)$}]{} edge from parent node [below]{$d$}}
edge from parent node [above]{$U$}
}
child[grow=right]{node(4)[solid node]{}
child{node[hollow node,label=right:{$(1,1)$}]{} edge from parent node [below]{$d$}}
child{node[hollow node,label=right:{$(3,0)$}]{} edge from parent node [above]{$u$}}
edge from parent node [above]{$D$}
}
edge from parent node [right]{$0.4$}
};

% information set
\draw[dashed,rounded corners=10]($(1) + (-.45,.45)$)rectangle($(2) +(.45,-.45)$);
\draw[dashed,rounded corners=10]($(3) + (-.45,.45)$)rectangle($(4) +(.45,-.45)$);
% specify mover at 2nd information set
\node at ($(1)!.5!(2)$) {$Demon$};
\node at ($(3)!.5!(4)$) {$Demon$};
\end{tikzpicture}
```

The game starts in the middle. Nature assigns Chooser to a type, and we move either up, if they are assigned $u$, or down, if they are assigned $d$. Then Chooser chooses an option. We move left if they choose $U$, and right if they choose $R$. Then Demon chooses, and we move up or down on the angled lines. The dotted lines around the two nodes are there because Demon doesn't know precisely which node they are at. They know what Chooser chose, the nodes inside the dashed lines are alike in that respect. But they don't know  which type assignment was made. And then we get the payouts, using the formulae in lines 6 and 7.

Note that while Demon can perfectly predict Chooser's strategy, it doesn't follow that they will perfectly predict Chooser's type. This can be true even if Chooser uses a non-probabilistic strategy. In particular, it is true if Chooser adopts what's called a pooling strategy, of playing the same option whatever type they are. If Chooser plays $U$ whether they are $u$ or $d$, the Demon will get no information from the play, and have to use their prior credence that Chooser has an 0.6 chance of being $u$. And so, it will be expected utility maximising for Demon to guess that Chooser is $u$, and that's what they will do. And the same goes for the situation where Chooser's strategy is to play $D$ no matter what.

The non-pooling strategies, on the other hand, are not stable. If Chooser gives Demon information about their type, that will mean Demon is more likely to accurately guess their type. And that's bad news for choosers who are of type $d$. But since Chooser knows their type when they act, they will not perform an act that's bad for their type. So they will not do anything other than play one of the two pooling strategies. (I'm glossing over a lot of the details here, but this is well worked out territory. See, inter alia, @ChoKreps1987 for the more careful version of the argument in this paragraph.)

So Chooser will play a pooling strategy. But which one will they play? Playing $U$ makes sense. If they are of type $u$, then they will get the best possible return, so they will be happy to follow through on the strategy. And if they are of type $d$, they will get a return of 2 from this strategy, and a return of 1 if they deviate from the strategy and play $D$. 




The best solution to beer-quiche puts constraints on priors

These aren't coherence constraints in any recognisable sense

But they are the kind of thing decision theory should take into account

So decision theory should take substantive notions of rationality into account

# Responding to Evidential Decision Theory


## So Why Ain't You Rich?

There is a familiar complaint against causal decision theory that goes back to the modern origins of decision theory in the 1970s. Here is a recent version of it due to @AhmedPrice2012. While their version is primarily directed against proceduralist forms of causal decision theory, this particular objection does not turn on the proceduralism. If the objection works, it also works against my defensivist version of causal decision theory. (I've slightly changed some of the wording, but otherwise this argumentis quoted from page 16 of their paper.)

1. In Newcomb problems, the average returns to one-boxing exceed that to two-boxing.
2. Everyone can see that (1) is true.
3. Therefore one-boxing foreseeably does better than two-boxing. (by 1, 2)
4. Therefore Causal Decision Theory (CDT) is committed to the foreseeably worse option for anyone facing Newcomb’s problem.

Here's what they, and many other proponents of Evidential Decision Theory (EDT) say follows from 4.

> The point of the argument is that if everyone knows that the CDT-irrational strategy will in fact do better on average than the CDT-rational strategy, then it’s rational to play the CDT-irrational strategy. @AhmedPrice2012, 17

This is what @Lewis1981e called the "Why Ain'cha Rich" argument, and what following @Bales2018 I'll call the WAR argument. I'm going to argue the last step of the WAR argument doesn't follow. Or, at the very least, that proponents of EDT cannot coherently say that it follows. For there are several cases where EDT foreseeably does worse than CDT. This section will go over three of them.

### Example One - Split Newcomb

This game takes place over three rounds. 

1. At stage one, the human player chooses Play or Exit. If they choose Out, player gets 5 and demon gets 1. If they choose In, we move onto stage two.
2. At stage two, demon chooses Left or Right, and this choice is announced.
3. At stage three demon and the player simultaneously choose either Up or Down. Demon is very good at predicting what player's choices will be, and indeed at stage two they were already very good at making such a prediction. And Demon wants to use these predictive powers to get as high a payoff as possible, and this is common knowledge.

If Demon chose Left at stage two, stage three involves the following game.

```{r}
left_anti_war <- tribble(
	   ~"", ~PU, ~PD,
	   "U", "$2, 1$", "$4, 0$",
	   "D", "$1, 0$", "$3, 3$"
	)
gameformat(left_anti_war, "The left hand side of Split Newcomb")
```

But if Demon chose Right at stage two, stage three involves this game.

```{r}
right_anti_war <- tribble(
	   ~"", ~PU, ~PD,
	   "U", "$12, 4$", "$14, 0$",
	   "D", "$11, 0$", "$13, 2$"
	)
gameformat(left_anti_war, "The left hand side of Split Newcomb")
```

If you'd prefer it as a game tree, here it is.

```{tikz, first-anti-war, fig.cap = "Tree Diagram of the Split Newcomb Game", fig.ext = 'png', cache=TRUE}
\tikzset{
    % Three node styles for game trees: solid, hollow, square
    solid node/.style={circle,draw,inner sep=1.5,fill=black},
    hollow node/.style={circle,draw,inner sep=1.5},
    square node/.style={rectangle,draw, inner sep = 1, fill = black}
}

\usetikzlibrary{calc}

\begin{tikzpicture}[scale=1.5,font=\footnotesize]
  % Specify spacing for each level of the tree
    \tikzstyle{level 1}=[level distance=15mm,sibling distance=45mm]
    \tikzstyle{level 2}=[level distance=15mm,sibling distance=45mm]
    \tikzstyle{level 3}=[level distance=15mm,sibling distance=10mm]
    \tikzstyle{level 4}=[level distance=12mm,sibling distance=10mm]
  % The Tree
  \node(0)[hollow node,label=left:{$C$}]{}
     child[grow=down]{node[solid node,label=below:{$(5,1)$}]{} 
        edge from parent node[right]{$E$}
        }
     child[grow=up]{node[solid node,label=above:{$D$}]{} 
        child[grow=left]{node[solid node,label=left:{$D$}]{} 
            child[grow=north west]{node(5)[solid node]{}
                child[grow=150]{node(1)[square node, label=left:{2,1}]{}
                    edge from parent node[above]{$U$}
                }            
                child[grow=210]{node(2)[square node, label=left:{1,0}]{}
                    edge from parent node[below]{$D$}
                }            
                edge from parent node[right]{$PU$}
            }
            child[grow=south west]{node(6)[solid node]{} 
                child[grow=150]{node(3)[square node, label=left:{4,0}]{}
                    edge from parent node[above]{$U$}
                }            
                child[grow=210]{node(4)[square node, label=left:{3,3}]{}
                    edge from parent node[below]{$D$}
                }            
                edge from parent node[right]{$PD$}
            }
            edge from parent node[above]{$L$}
        }
        child[grow=right]{node[solid node,label=above:{$D$}]{} 
            child[grow=north east]{node(11)[solid node]{}
                child[grow=30]{node(7)[square node, label=right:{12,4}]{}
                    edge from parent node[above]{$U$}
                }            
                child[grow=330]{node(8)[square node, label=right:{14,0}]{}
                    edge from parent node[below]{$D$}
                }            
                edge from parent node[left]{$PU$}
            }
            child[grow=south east]{node(12)[solid node]{} 
                child[grow=30]{node(9)[square node, label=right:{11,0}]{}
                    edge from parent node[above]{$U$}
                }            
                child[grow=330]{node(10)[square node, label=right:{13,2}]{}
                    edge from parent node[below]{$D$}
                }            
                edge from parent node[left]{$PD$}
            }
            edge from parent node[above]{$R$}
        }
        edge from parent node[left]{$P$}
      }
;

  \draw[dashed,rounded corners=10]($(5) + (-.2,.25)$)rectangle($(6) +(.2,-.25)$);
  % specify mover at 2nd information set
  \node at ($(5)!.5!(6)$) {$C$};

  \draw[dashed,rounded corners=10]($(11) + (-.2,.25)$)rectangle($(12) +(.2,-.25)$);
  % specify mover at 2nd information set
  \node at ($(11)!.5!(12)$) {$C$};

\end{tikzpicture}

```

We start at the hollow node in the middle, and Chooser (here denoted as 'C') either goes up by Playing, or goes down by Exiting. Then Demon moves Left or Right. Then Demon moves again, making a prediction. But this second move isn't revealed to Chooser, which is why on either side Chooser's nodes are in an information set. That's to say, Chooser chooses Up or Down knowing whether Demon has gone Left or Right, but not knowing whether Demon has predicted Up or Down. And then we get the payoffs.

Whether Demon goes Left or Right, the CDTer will choose Up, and the EDTer will choose Down. Either choice Chooser faces is a fairly straightforward Newcomb Problem. In both sub-games Up causally dominates Down, but Down will get a higher return if you assume, as we did assume, that demon mostly makes correct predictions.

So at stage two, Demon will know that if the person facing them is an EDTer, they will get a return of 3 from Left and 2 from Right. (They'll end up in the Down-Down cell either way.) So they will rationally choose Left. On the other hand, if the person facing them is a CDTer, they will get a return of 1 from Left and 4 from Right. (They'll end up in the Up-Up cell either way.) So they will rationally choose Right. And everything in this paragraph can be deduced by a rational player at stage 1.

So at stage one, a CDTer will know that if they Play, they expect to get 12 (the game will go Right then Up-Up), and if they Exit, they know they'll get 5. So they'll Play. But an EDTer will know that if they Play, they expect to get 4 (the game will go Left then Down-Down), and if they Exit, they know they'll get 5. So they'll Exit.

The result of all this is that the CDTer will get 12, and the EDTer will get 5. So the CDTer will predictably do better than the EDTer. Indeed, the EDTer will voluntarily choose at stage one to take a lower payout than the CDTer ends up with. This seems bad for EDT, at least if we think that predictably ending up with a lower outcome is bad.

Now you might object that this is because at stage two the demon chooses to treat the EDTer differently to how they treat the CDTer. I don't really agree for two reasons, though I'm not sure either of these reasons work. (Hence the second and third examples that are about to come.) One is that the demon isn't trying to harm the EDTer; they are just trying to maximise their return. It so happens that EDT is such an impermissive theory that it doesn't allow for any flexibility, and the Demon, knowing this, is forced to take choices that are bad for EDT, and indeed worse for Demon than if they ended up at Right-Up-Up. But this isn't Demon's fault; it's the fault of EDT being so impermissive. The other reason is that Demon does not in fact make any choices that hurt the EDTer. The EDTer should expect that Demon will in fact make such choices, in response to their theory, but that's not quite the same thing. The only player who moves at all in the EDT version of the game is Chooser. So it's a little hard to say this is just a case where the EDTer is harmed by the demon's malicious choices.

I think those responses work, but I'm not completely sure that they do. So let's look at a different example, one where Demon doesn't have these variable payouts.

### Example Two - Coins and Signals {#coinssignals}

This example is a version of a signaling game of the kind introduced by @Lewis1969a. And in particular it's a version of the broadly adversarial kinds of signaling games that are central to the plot of @ChoKreps1987, and which we discussed a lot in chapter \ref@(coherent). Again, it will involve three stages.

At the first stage a fair coin is flipped, and the result shown to Chooser, but not to Demon.

At the second stage, Chooser will choose Up or Down, and the choice will be publicly announced.

At the third stage, Demon will try to guess what the coin showed. Demon knows the payoff table I'm about to show you, and is arbitrarily good at predicting Chooser's strategy. That is, Demon can make accurate predictions of the form "If Heads, Chooser will make this choice, and if Tails, they will make that choice."

The payoffs to each player are a function of what happens at each of the three steps, and are given by the following table.

 Coin   Chooser   Demon   Chooser Payoff   Demon Payoff  
------ --------- ------- ---------------- --------------
   H      U        H       40               1
   H      U        T       400              0
   H      D        H       0                1
   H      D        T       0                0
   T      U        H       40               0
   T      U        T       28               1
   T      D        H       28               0
   T      D        T       36               1

In tree form, here is the game they are playing.

```{tikz, second-anti-war, fig.cap = "Tree Diagram of the Coins and Signals Game", fig.ext = 'png', cache=TRUE}
\usetikzlibrary{calc}

\begin{tikzpicture}[scale=1.4,font=\footnotesize]
\tikzset{
% Two node styles for game trees: solid and hollow
solid node/.style={circle,draw,inner sep=1.5,fill=black},
hollow node/.style={circle,draw,inner sep=1.5},
square node/.style={rectangle,draw, inner sep = 1, fill = black}
}

% Specify spacing for each level of the tree
\tikzstyle{level 1}=[level distance=12mm,sibling distance=25mm]
\tikzstyle{level 2}=[level distance=15mm,sibling distance=15mm]
\tikzstyle{level 3}=[level distance=13mm,sibling distance=11mm]

% The Tree
\node(0)[hollow node,label=above:{Nature}]{}
child[grow=left]{node[solid node,label=left:{
$Chooser$
}] {}
child[grow=up]{node(1)[solid node]{}
child{node[square node,label=above:{$400,0$}]{} edge from parent node [right]{$T$}}
child{node[square node,label=above:{$40,1$}]{} edge from parent node [left]{$H$}}
edge from parent node [left, yshift = -5]{$Up$}
}
child[grow=down]{node(3)[solid node]{}
child{node[square node,label=below:{$0,1$}]{} edge from parent node [left]{$H$}}
child{node[square node,label=below:{$0,0$}]{} edge from parent node [right]{$T$}}
edge from parent node [left, yshift = 5]{$Down$}
}
edge from parent node [below, align=center]{$H$ \\ $0.5$}
}
child[grow=right]{node[solid node,label=right:{
$Chooser$
}] {}
child[grow=up]{node(2)[solid node]{}
child{node[square node,label=above:{$28,1$}]{} edge from parent node [right]{$T$}}
child{node[square node,label=above:{$40,0$}]{} edge from parent node [left]{$H$}}
edge from parent node [right, yshift = -5]{$Up$}
}
child[grow=down]{node(4)[solid node]{}
child{node[square node,label=below:{$28,0$}]{} edge from parent node [left]{$H$}}
child{node[square node,label=below:{$36,1$}]{} edge from parent node [right]{$T$}}
edge from parent node [right, yshift = 5]{$Down$}
}
edge from parent node [below,align=center]{$T$ \\ $0.5$}
};

% information set
\draw[dashed,rounded corners=10]($(1) + (-.45,.45)$)rectangle($(2) +(.45,-.45)$);
\draw[dashed,rounded corners=10]($(3) + (-.45,.45)$)rectangle($(4) +(.45,-.45)$);
% specify mover at 2nd information set
\node at ($(1)!.5!(2)$) {$Demon$};
\node at ($(3)!.5!(4)$) {$Demon$};
\end{tikzpicture}


```

Demons's payoffs are just as you'd expect - they get rewarded iff they figure out how the coin landed. Chooser's payoffs are more complicated, but the big thing to note is they get the biggest rewards if they manage to play Up while Demon makes an incorrect prediction.

One last thing to stipulate about Demon before we analyse the game. If Demon predicts Chooser will do one thing if Heads and another if Tails, they will use the information from Chooser's choice to make their guess about how the coin landed. But if they predict Chooser will say the same thing whether the coin landed Heads or Tails, they won't know how the coin landed, and will flip their own coin to make a guess. So in that case it will be 50/50 whether Demon says Heads or Tails.

Onto the analysis. It should be fairly clear that if the coin lands Heads, the human should say Up. The worst possible return from Up is 40, the best possible return from Down is 0. So that's what both a CDTer and an EDTer would do, and hence what Demon would predict that they will do.

So what happens if the coin lands Tails? Given Demon will predict Up if Heads, we can work out the value of Up and Down if Tails to the EDTer. If they play Up, Demon will predict that, and hence Demon will flip a coin to choose Heads or Tails. So they have a 50/50 shot at getting either 40 or 28, and so their expected return is 34. If they play Down, Demon will predict that, and hence Demon will say Tails, and they will get a return of 36. Since 36 \> 34, they will play Down if Tails.

That's the unique solution to the game for the EDTer. They play Up if Heads, Down if Tails. Demon can figure out that they'll do this, so will correctly guess what the coin showed. And they will get 40 if the coin landed Heads, and 36 if it landed Tails, for an expected return of 38.

What should the CDTer do? And, in particular, what should a causal defensivist do? Well, it turns out this is another problem where the theory is not decisive. Doing exactly what the EDTer does is defensible. But it's also defensible to say Up no matter what. Let's go over why this is defensible. The question is whether Chooser can endorse their decision to play Up no matter what after each possible result of the coin toss. They can clearly endorse it if the coin lands Heads; in that case Up strictly dominates Down, and strictly dominant are always defensible. What if the coin lands Tails? Well they think they'll play Up. So they think the demon will flip a coin to guess in this situation. So they think the expected return of Up is 34 (like the EDTer thinks), and the expected return of Down is 32. The key difference here is that when working out the expected return of a non-chosen option, the Chooser who believes in causal defensivism does not change the expected behavior of the demon, while the EDTer does. (This disposition is why dominance reasoning works for them.) So Chooser will think that even if the coin lands Tails, they would do worse on average if they switched to playing Down if Tails. So it follows that they can defensibly play Up either way.

And if they do play this, the rewards are handsome. The demon won't have any information about the coin, so the demon will flip their own coin. So lines 1, 2, 5 and 6 of the table are all equally likely to appear. So if Chooser plays this strategy, they are equally likely to get a return of 40, 400, 40 or 28, for an overall expected return of 127. And this is much higher than the 38 the EDTer is expected to receive. By changing the payout on line 2 of the table, we can make the gap in expected returns be arbitrarily large.

Now you might object that while the the causal defensivist can do better, it doesn't follow that EDT is wrong. After all, we've just said here that a rival theory may do better. I don't think that matters much. The point of the WAR is to refute a theory, and if the EDTer does foreseeably worse than one kind of Chooser who follows causal defensivism, that should be enough to refute them. But just in case you think this objection is stronger, we'll include one last example.

### Example Three - Coins and Newcomb

This is just like Example Two, with one twist. If the game goes Tails, Down, Tails, then we don't immediately end the game and make payouts to the players. Instead we play another game, with a familiar structure. As always, Demon is really good at predicting Chooser's play, and Chooser's payouts are listed first in every cell. (I'm not going to include a tree here, because it is more confusing than helpful.)

        Up       Down
----- -------- --------
   Up  20, 1    40, 0
 Down  16, 0    36, 1

The EDTer will think they'll get 36 from this game, so the example will be just like Example Two. And the EDTer will play Up if Heads, Down if Tails, for an expected return of 38.

But if Chooser follows causal defensivism, then they will think that if the game gets to this stage, they'll get 20. So now they think that in the original game, Up dominates Down no matter whether the coin lands Heads or Tails. So now they will definitely play Up no matter what, and get an expected return of 127.

### Why The Examples Matter

I've argued against EDT elsewhere in the book, but note that this section is very much not an argument against EDT; instead, it's part of a war on WAR. The point of the first example is that any theory whatsoever is subject to a WAR argument. That's because for any theory whatsoever, you can construct pairs of choices like Left and Right, where the theory says to take choices that lead Demon to preferring to go Left. So for any theory whatsoever, or at least any theory that is consequentialist in the sense popularised by @Hammond1988, there is an example where the theory leads to worse returns. So any consequentialist theory is subject to an objection by WAR. It's the paradigm of an over-generating objection.

There is perhaps something a bit interesting about the second example, though it isn't a problem especially for EDT. What makes the second example work is that Chooser is in a situation that rewards unpredictability, but EDT is decisive, and hence predictable. And any decisive theory will be subject to an WAR-style objection from cases like this. Now this isn't part of my argument for indecisiveness, since I think WAR-style objections are bad. But following the recipe from subsection \@ref(coinssignals), it is possible to construct a case where any decisive theory will lead to predictably bad outcomes. It's always incoherent to endorse the combination of WAR reasoning and decisive decision theories. Even though I reject both of these, I think it's pretty clear in this case that WAR reasoning should be the first to go. And with it goes not EDT, but one of the historically key arguments for EDT. Let's turn to a more contemporary argument for EDT next.




## To Bet the Impossible Bet

The Ahmed cases all go away if you focus on what's possible at the end of deliberation, not at the start of it

This might require some contextualism about ability, a la Hawthorne and Pettit, to really make stick

See page 63 of Joyce book for the premise that counterfactuals about the bet must be specified in order for us to have a real bet.

See also Joyce's review of Ahmed for much more on this

# Epistemic Uniqueness and Decisiveness

## Uniqueness and Permissivism

In chapter \@ref(decisive) I argued that decision theory is often compatible with multiple unequal solutions to games. And in chapter \@ref(coherence) I argued that decision theory should be sensitive to what credences the chooser should have, not just the credences they actually have. If you add to these views the philosophical view known as Uniqueness, you get something close to a contradiction. Uniqueness says, roughly, that in any situation there is a unique set of credences one should have. To keep my views coherent, I need to endorse its negation: Permissivism, which says that rationality is compatible with a number of distinct attitudes in particular situations.

This chapter argues that thinking about symmetric games gives us new reason to believe in permissivism. In some finite games, if permissivism is false then we have to think that a player is more likely to take one option rather than another, even though each have the same expected return given that player's credences. And in some infinite games, if permissivism is false there is no rational way to play the game, although intuitively the games could be rationally played. The latter set of arguments rely on the recent discovery that there are symmetric games with only asymmetric equilibria. It was long known that there are symmetric games with no pure strategy symmetric equilibria; the surprising new discovery is that there are symmetric games with asymmetric equilibria, but no symmetric equilibria involving either mixed or pure strategies.

The permissivist theses that have been the focus on recent philosophical attention vary along two dimensions.^[For a much more thorough introduction to the debate, and especially into the varieties of permissivist theses, see @KopecTitelbaum2016. Much of the setup here, including for example the use of the subjective Bayesian as an illustrative example, is from that paper.]

The first dimension concerns what we hold fixed when we say that multiple attitudes are rationally permissible. The weakest possible theory just says that two people with distinct attitudes may both be rational. No one really denies this. The strongest theory says that holding every fact about a situation constant, there are two possible rational attitudes. In between we have a number of interesting theses. For instance, we can ask whether multiple attitudes are rationally compatible holding constant the evidence the believer has. And we can ask whether multiple attitudes are rationally compatible holding constant both the evidence and the believer's prior doxastic states. A classic form of subjective Bayesianism answers _yes_ to the first question, and _no_ to the second. The focus here will be on a thesis very close to the strongest one - whether two people who are alike in all qualitative respects can rationally have different attitudes.

The second dimension concerns whether the folks holding these distinct attitudes can acknowledge that rival attitudes are rational. Some permissivists hold that distinct attitudes can be rationally compatible with holding fixed evidence or priors or whatever, but the people holding these attitudes cannot acknowledge that attitudes other than theirs are rational. The argument I'm going to offer draws the stronger conclusion that multiple responses are rational, and rational thinkers can acknowledge that alternative responses to theirs are rational.

The negation of a permissive thesis is a Uniqueness thesis. The name suggests that there is precisely one rational attitude to take in a specified situation, but we'll interpret it as the view that there is at most one rational attitude to take so as to ensure each Uniqueness thesis is the negation of a permissive thesis. As with Permissivism, Uniqueness comes in weaker and stronger varieties. The strongest version is the literally incredible view that there is only one doxastic attitude that is rationally permissible. (Presumably it is the view that is certain of all and only truths.) The weakest version, which is still interesting, is that once a situation is described in full detail, there is precisely one doxastic attitude that is rationally permissible. Everyone holds that, since the normative supervenes on the descriptive, that describing a situation in full detail fixes which doxastic states are rationally permissible. The Uniqueness theorist adds the claim that there are 0 or 1 such states.^[Two small caveats here. Uniqueness theorists may say that it is permissible to not have any attitude towards a proposition. So it is consistent with Uniqueness, as I understand it, to say that it could be both rational to have credence 0.6 in $p$, and rational to not have any attitude towards $p$. What the Uniqueness theorist denies is that there are distinct credences towards $p$ one could adopt, each of which would be rational. And of course the Uniqueness theorist thinks it could be rational to have credence 0.6 in $p$ and 0.7 in $q$. When I say Uniqueness implies that just one state is rational, I mean to quantify over complete credal states, not attitudes towards single propositions.]

As I said, I'm interested in defending a strong, but not maximally strong, version of Permissivism. Equivalently, I'm interested in attacking a weak, but not quite maximally weak, version of Uniqueness. Here is the version of Uniqueness that I want to reject.

- For all kinds $K$, and evidence $E$, there is at most one credal distribution that is rational for an agent of kind $K$ with evidence $E$.

I mean to be fairly liberal over what counts as a \`kind', so if any such theory is false, we have proven that a lot of Uniqueness theories are false. So a kind could be a prior probability function, a set of privileged predicates that one uses for induction, an attitude to inductive risk, and so on. The only assumption I'll make is that kinds are shareable; so there is no such thing as the kind _Being John Malkovich_. In principle we could say that what evidence one has is part of one's kind, but the discussion below will be clearer if we separate out evidence and kinds.

The next two sections set out two symmetric games where Uniqueness leads to surprising results. I think the results are surprising, indeed implausible, enough that we should reject Uniqueness. But even if one doesn't accept that, it's still interesting to see what Uniqueness entails. In all cases we'll assume that the following things are common knowledge among players of the game.

- Each player is rational, so they form rational credences, and maximise expected utility.
- Each player is of kind $K$.
- Each player knows the payout structure  of the game.
- Each player is self-aware; they know their own credences.
- If Uniqueness is true, then each player knows that Uniqueness is true.
- Each player has no other relevant evidence about the game or the players.

Let evidence $E$, unless otherwise stated, be the evidence specified by those six bullet points. We'll be continually thinking about propositions of the form:

- A rational agent of kind $K$ with evidence $E$ will perform action $varphi$ in this game.

Since the games are symmetric, we don't have to ask about which player will make this move; we can think abstractly about what any rational player would do.

## Chicken

Some finite symmetric games have no symmetric pure-strategy equilibria. One notable example is Chicken. Here's a version of Chicken that will do.

```{r, simple-chicken}
chicken <- tribble(
	   ~"", ~Stay, ~Swerve,
	   "Stay", "-100, -100", "1, -1",
	   "2", "1, -1", "0, 0"
	)
gameformat(chicken, "Chicken")
```

The symmetric pure-strategy pairs $\langle$Stay, Stay$\rangle$ and $\langle$Swerve, Swerve$\rangle$ are not equilibria; in each case both parties have an incentive to defect. But the game does have a symmetric equilibria. It is that both players play the mixed strategy of Stay with probability 0.01, and Swerve with probability 0.99. 

Let **Swerve** be the proposition that a rational player of kind $K$ with evidence $E$ will Swerve. And call the players Row and Column. Given our assumptions so far, plus Uniqueness, we can prove that Row's credence in **Swerve** is 0.99. Here's the proof.

1. Let $x$ be Row's credence in **Swerve**.
2. By self-awareness, Row knows that $x$ is her credence in **Swerve**.
3. Since she knows she is rational, Row can infer that $x$ is a rational credence in **Swerve**.
4. Since she knows Uniqueness is true, Row can infer that $x$ is the only rational credence in **Swerve**.
5. Since she knows Column is rational, she can infer that $x$ is Column's credence in **Swerve**.
6. Since all the assumptions so far are common knowledge, she can infer that Column knows that $x$ is her credence in **Swerve**.
7. If $x = 1$, then Row can infer that it is rational for Column to Swerve, while knowing that Row will also Swerve. But this is impossible, since if Column knows Row will Swerve, it is best to Stay. So $x \neq 1$.
8. If $x = 0$, then Row can infer that it is rational for Column to Stay, while knowing that Row will also Stay. But this is impossible, since if Column knows Row will Stay, it is best to Swerve. So $x \neq 0$.
9. So $0 < x < 1$.
10. Since Row knows Column's credence that Row will Swerve (whatever it is), and Row knows Column is rational, but Row does not know what Column will do, it must be that Column is indifferent between Stay and Swerve given her credences about what Row will do.
11. Column is indifferent between Stay and Swerve only if her credence that Row will Swerve is 0.99. (This is a reasonably simple bit of algebra to prove.)
12. So from 10 and 11, Column's credence that Row will Swerve is 0.99.
13. By (known) Uniqueness, it follows that the only rational credence in **Swerve** is 0.99.
14. So since Row is rational, it follows that $x = 0.99$.

Now there is nothing inconsistent in this reasoning. In a sense, it is purely textbook reasoning. But the conclusion is deeply puzzling. We've proven that Column is indifferent between her two options. And we've proven that Row knows this. But we've also proven that Row thinks it is 99 times more likely that Column will choose one of the options over the other. Why is that? It isn't because there is more reason to do one than the other; given Column's attitudes, the options are equally balanced. It is purely because Uniqueness pushes us to a symmetric equilibrium, and this is the only symmetric equilibrium.

I don't think this result will convince many devotees of Uniqueness to give up their view. It's not a particularly novel claim that rational players will end up at the unique Nash equilibrium of a game. And to be sure, if this game was being played repeatedly, much weaker assumptions entail that each player should Stay 1% of the time, with those Stays being randomly distributed across the plays of the game. But it is still odd, at least to me, to see the same conclusion drawn in the single shot game, where each player is known to be indifferent between their choices.

The next case is I think much worse for Uniqueness.

## Elections

The cases that really inspired this chapter come from some recent work on this rather old question,

> If a symmetric game has an equilibrium, does it have a symmetric equilibrium?

Over the years, a positive answer was given to various restricted forms of that question. Most importantly, John @Nash1951 showed that if each player has finitely many moves available, then the game does have a symmetric equilibrium. 

But recently it has been proven that the answer to the general question is no. Mark @Fey2012 showed that there are symmetric positive-sum two-player games that have only asymmetric equilibria.^[Fey also includes a nice chronology of some of the proofs of positive answers to restricted forms of the question.] And Dimitrios @Xefteris2015 showed that there is a symmetric three-player zero-sum that has only asymmetric equilibria. In fact, he showed that a very familiar game, a version of a Hotelling–Downs model of elections, has this property. Here's how he describes the game.

> Consider a unit mass of voters. Each voter is characterized by her ideal policy. We assume that the ideal policies of the voters are uniformly distributed in [0, 1]. We moreover assume that three candidates $A$, $B$ and $C$ compete for a single office. Each candidate $J \in \{A, B, C\}$ announces a policy $s_J \in [0, 1]$  and each voter votes for the candidate who announced the policy platform which is nearest to her ideal policy. If a voter is indifferent between two or among all three candidates she evenly splits her vote between/among them. A candidate $J \in \{A, B, C\}$ gets a payoff equal to one if she receives a vote-share strictly larger than the vote-share of each of the two other candidates. If two candidates tie in the first place each gets a payoff equal to one half. If all three candidates receive the same vote-shares then each gets a payoff equal to one third. In all other cases a candidate gets a payoff equal to zero. @Xefteris2015, 124

It is clear that there is no symmetric pure-strategy equilibrium here. If all candidates announced the same policy, everyone would get a payoff of $\frac{1}{3}$. But no matter what that strategy is, if $B$ and $C$ announce the same policy, then $A$ has a winning move available. 

What's more surprising, and what Xefteris proves, is that there is no symmetric mixed strategy equilibria either. Again, in such an equilibrium, any player would have a payoff of $\frac{1}{3}$. Very roughly, the proof that no such equilibrium exists is that random deviations from the equilibrium are as likely to lead to winning as losing, so they have a payoff of roughly $\frac{1}{2}$. So there is no incentive to stay in equilibrium. So no symmetric equilibrium exists.

But if Uniqueness is true, and if it is possible to play the game under circumstances of common knowledge of rationality and kind, then there must be a symmetric equilibrium. The reason is that a version of the proof of the previous section still goes through. Whatever credal distribution $A$ has over $B$'s possible policies, $A$ must also have over $C$'s policies (since they both adopt the uniquely rational strategy), and she must know that $B$ and $C$ each have over each other's policies and over hers, and these distributions must be consistent with each player having these credal distributions while thinking that the other players have the same distributions and are maximising expected utility. In other words, the assumptions we've made about the game imply that $A$ has a credal distribution $F$ over $B$'s possible policies only if the mixed strategy triple where each player adopts $F$ as their mixed strategy is itself an equilibrium. And that would be a symmetric equilibrium. But no symmetric equilibrium exists. 

But if we drop Uniqueness, it is possible to keep all the other assumptions. As Xefteris points out, the game has asymmetric equilibria. Here is one possible model for the game.

- $A$ plays 0.6 (and wins), $B$ and $C$ each play 0.4 (and lose).
- Each player has a correct belief about what the other players will play.
- But both $B$ and $C$ know they cannot win given the other player's moves, so they pick a move completely arbitrarily.
- Further, each player has a correct belief about why each player makes the move they make.

This is the coherent equilibria that Xefteris describes, but note that it is rather implausible that we'd end up there in a real-life version of the game. It requires two of the players to know that one of the other players will be indifferent between their options, but from this draw a correct inference about what they will do. That's not particularly plausible. So let's note that there is a somewhat more plausible way to get all three players to make those moves.

- $A$ plays 0.6 (and wins), $B$ and $C$ each play 0.4 (and lose).
- The only two rational plays are 0.4 and 0.6, but each of them is permissible.
- In any world that a player believes to be actual, or a player believes another player believes to be actual, or a player believes another player believes another player believes to be actual, etc., the following two conditions hold.
- If a player plays 0.6, they believe the other two players will play 0.4, and hence playing 0.6 is a winning move.
- If a player plays 0.4, they believe the other two players will play 0.6, and hence playing 0.4 is a winning move.

The main difference between this model and Xefteris's is that it allows that players have false beliefs. But why shouldn't they have false beliefs? All they know is that the other players are rational, and rationality (we're assuming) does not settle a unique verdict for what players will do.^[To use the game-theory jargon, Xefteris describes a Nash equilibrium of the game, but what I've described is a a rationalizable strategy triple [@Bernheim1984, Pearce1984]. If Uniqueness is true, then strictly speaking any rationalizable strategy pair for a symmetric game is a Nash equilibrium.] So I think this strategy set, where the players have rational (but false) beliefs about the other players, is more useful to think about.

## Objections

The reductio arguments here have all assumed not just that Uniqueness is true, but that the players know that it is true. What happens if we drop that assumption, and consider the possibility that Uniqueness is true but unknowable?

This possibility is a little uncomfortable for philosophical defenders of Uniqueness. If the players in these games do not know that Uniqueness is true, then neither do the authors writing about Uniqueness. And now we have to worry about whether it is permissible to assert in print that Uniqueness is true. I wouldn't make too much of this though. It is unlikely that a knowledge norm governs assertion in philosophical journals.

The bigger worry here is that one key argument for Uniqueness seems to require that Uniqueness is knowable. A number of recent authors have argued that Uniqueness best explains our practice of deferring to rational people.^[There is a nice discussion of this argument, including citations of the papers I'm about to discuss, in @KopecTitelbaum2016 [195].] For instance, Greco and Hedden use this principle in their argument for Uniqueness.

> If agent $S_1$ judges that $S_2$'s belief that $P$ is rational and that $S_1$ does not have relevant evidence that $S_2$ lacks, then $S_1$ defers to $S_2$'s belief that $P$. @GrecoHedden2016, 373.

Similar kinds of arguments are made by @Dogramaci2012 and @Horowitz2014. But the principle looks rather dubious in the case of these games. Imagine that $A$ forms a belief that $B$ believes that a rational thing to do in the Xefteris game is to play 0.6, and so she will play 0.6. She should judge that belief to be rational; as we saw it is fully defensible. But although she does not believe that she has evidence that $B$ lacks, she should not defer to it. At least, she should not act as if she defers to it; believing that $B$ will play 0.6 is a reason to play something other than 0.6. 

And that's the general case for these symmetric games with only asymmetric equilibria. Believing that someone else is at an equilibrium point is a reason to not copy them. If it were not a reason to not copy them, then the strategy profile where each player plays the same thing would be a symmetric equilibrium. So thinking about these games doesn't just give us a rebutting defeater for Uniqueness, as described in the previous two sections, but an undercutting defeater, since they also tell against a premise that has been central to recent defences of Uniqueness.

I think there is a somewhat better move available to the Uniqueness theorist. They could simply deny that the Xefteris game, as I've described it, is even possible. ^[This is really just a response to the argument based on that game; I think they just have to say that in Chicken a rational player will rationally think the other player is more likely to make one of the two choices with equal expected payoffs.] This perhaps isn't as surprising as it might seem.

Note two things about the Xefteris game. First, it is an infinite game in the sense that each player has infinitely many choices. It turns out this matters to the proof that there is no symmetric equilibrium to the game. Second, we are assuming it is common knowledge, and hence true, that the players are perfectly rational. Third, we are assuming that perfect rationality entails that people will not choose one option when there is a better option available. When you put those three things together, some things that do not look obviously inconsistent turn out to be impossible. Here's one example of that.

> $A$ and $B$ are playing a game. Each picks a real number in the open interval (0, 1). They each receive a payoff equal to the average of the two numbers picked.

For any number that either player picks, there is a better option available. It is always better to pick $\frac{x+1}{2}$ than $x$, for example. So it is impossible that each player knows the other is rational, and that rationality means never picking one option when a better option is available. 

So the Uniqueness theorist could say that the same thing is going on in the Xefteris game. Some infinitely games cannot be played by rational actors (understood as people who never choose sub-optimal options); this is one of them. But if this is all the Uniqueness theorist says, it is not a well motivated response. We can say why it is impossible to rationally play games like the open interval game; the options get better without end. But that isn't true in the Xefteris game. The only thing that makes the game seem impossible is the Uniqueness assumption. People who reject Uniqueness can easily describe how the Xefteris game can be played by rational players. Simply saying that it is impossible, without any motivation or explanation for this other than Uniqueness itself, feels like an implausible move.

## Conclusion

If Uniqueness is true, then the following thing happens in games between people who know each other to be the same kind, and to be rational. When someone forms a belief about what the other person will do, they can infer that this is a rational way to play the game given knowledge that everyone else will do the same thing. But sometimes this is a very unintuitive inference. In Chicken, it implies that we should have asymmetric attitudes to someone who is facing a choice between two options with equal expected value. In the election game Xefteris describes, a game that feels consistent turns out to be impossible. 

I think the conclusion to draw from these cases of symmetric interactions this is that Uniqueness is false, and hence permissivism is true. Sometimes in such an interaction one simply has to form a belief about the other player, knowing they may well form a different belief about you. Indeed, sometimes only coherent way to form a belief about the other player is to believe that they will form a different belief about you. And that means giving up on Uniqueness. And if we give up on Uniqueness, then there is no tension between the views in chapters \@ref(decisive) and \@ref(coherence).

# Puzzles about Weak Dominance

## Why Weak Dominance

Basic ABC example. Demon might not give a choice but if they do Up beats Down

This doesn’t require weak dominance but that’s the simplest explanation

Also fits with the “can you defend this”

But two puzzles for the next two sections

## Three Kinds of Demon

Set up red green game and properly cite

Note this is my third try at it

First demon, limit prob. This one is easy

Second demon, zero prob but possible. Answer one this can’t happen. But this takes us into infinitesimal territory and I’m not going there. Answer two, the speech sounds bad. Don’t take uncompensated risks. 

Third demon, can not fail.  Then not in fact weak dominance post choice bc alternative is not in fact possible. 

Same goes for symmetric humans but third is really impossible. 

## Iterated Weak Dominance

Sometimes thought that if WD then committed to IWD. Cite HH and Stalnaker in reply. 

Three objections 
1. Order effects
2. Gets unintuitive results
3. Nothing incoherent about speeches that violate

Start with Bonanno on order effects

Do strategic version of money burning and show what IWD leads to
Note that nothing wrong with all H speech
This might get complicated

Now do dynamic version 
Really absurd that having an untaken option can make a difference, when others know you won’t take it
But does HH mean that you think demon is stupid? No it means that you think demon might follow up stupid with stupid
Also do this using counterfactuals maybe 

# (APPENDIX) Appendicies {-} 

# Game Theory {#gametheory}

This appendix is a brief introduction to some important game theoretic concepts. It won't introduce anything new to anyone who has studied practically any game theory before. But since that's not true of all philosophers, I thought it was helpful to briefly introduce the basics here.

# About a Demon {#aboutademon}

Include stuff about arbitrarily accurate
Note that known perfect accuracy leads to challenges, one's we'll come back to in...

Also note that we're assuming the demon predicts the strategy a player chooses. That could include randomisation.

Note that I'm simply assuming expected utility theory is right for non-demonic problems.

But that does imply an assumption that there is some reason for that. And I think that's got to include something like a Sure Thing Principle.

But I am including the open box Newcomb Problem as demonic.

And maybe a riff on why it's Player/Chooser/Agent whatever

# Methodology {#methodology}

There are three primary sources of evidence that are available in constructing and evaluating decision theories: principles, sameness of cases, and cases. Most of the literature focusses on the third of these. I think that's a mistake; this is by far the least reliable source of evidence. We should instead focus on the first two. That's what I'll do in this book, and the purpose of this section is to defend this methodological starting point.

By principles, I mean claims like _Preferences should be transitive_, or _Choosers should not regret their choices as soon as they are made_. These are very general claims about what the structure of one's choices should look like. There aren't a lot of principles like these that we can be antecedently very confident in. But there are some, and those we should hold on to barring extraordinary evidence.

By sameness of cases, I mean claims that two particular decisions should get (in some sense), the same choice. Here is a familiar example from a non-Demonic decision problem. Chooser has a ticket to today's cricket match, and is deciding whether to go. Chooser enjoys watching cricket, but does not enjoy sitting around in the stands waiting for the rain to clear, and there is a good chance of rain. What should Chooser do? Well, we haven't said nearly enough to settle that, so let's ask something more precise. What more do we need to know to know what Chooser should do? We do need to know how much Chooser likes watching cricket, dislikes sitting around in the rain, will have to pay to get to the ground and how likely rain is. But we don't need to know how much Chooser paid for the ticket. That's a sunk cost. If we settle all the forward looking parameters (likelihood of rain, utility of going under different scenarios, etc), then changing the backwards looking ones (how much Chooser paid) doesn't make a difference. If it would be rational to stay home given all those parameters and having paid $10 for the ticket, it would be rational to stay home given all those parameters and having paid $100 for the ticket. Even in hard cases, and if the weather is bad enough Chooser may have a very hard choice here, we often have clear enough knowledge that some differences do not in fact make a difference.

And by cases I mean claims about what Chooser should do in a particular vignette. These are the main form of evidence that get used in philosophical decision theory. We (the theorists) are told that Chooser faces a problem like the following choice between $A$ and $B$, with a Demon predicting the choice and the payout dependent on the actions of the two of them.

```{r, basic-stag, cache=TRUE}
basic_stag <- tribble(
	   ~"", ~PA, ~PB,
	   "A", "$6$", "$0$",
	   "B", "$4$", "$3$"
	)
gameformat(basic_stag, "An example of a choice situation")
```

A lot of philosophers seem to the following approach to decision theory. (I say 'seem to' because I've never really seen a good defence of this methodology, but I have seen a lot of arguments from cases like this to sweeping theoretical claims.) First, we figure out what Chooser should do in this case, perhaps by consulting our intuitions. Second, we work out which theory best fits with what we've learned this way about individual cases.

Now it would be wrong to say that the three kinds of evidence I've presented here fall into three very neat categories. The boundaries between them are blurry at best. Any evidence about an individual case can be turned into a kind of principle by simply replacing the names in the vignette with variables and universally quantifying over the variables. We'll get very restricted principles that way - anyone facing just that game should play $B$, for example - but all principles have some restrictions on them. And there's not much distance between judging that what Chooser should do is independent of what they paid for the ticket, and judging that the principle _Sunk costs are irrelevant_ is part of our evidence. But the fact that a boundary isn't sharp doesn't mean that it's theoretically useless. The paradigms of the three kinds of evidence are different enough that we can helpfully keep them in mind when understanding what particular theorists are doing.

And the paradigm that starts with individual cases, like the table I just presented, seems considerably worse than the other two paradigms. Arguments in that family are vulnerable to two kinds of objection that arguments that start with the other kinds of evidence are not.

First, judgments about cases might conflate what the standards of ideal rationality say about the case with what the standards of non-ideal rationality say about the case. Consider this example, which I'll come back to in chapter \@ref(dilemma). 


> **Quick Basketball Choice**    
> Chooser has to name an NBA team. (The NBA is the main club basketball competition in the world.) If the team wins the NBA championship this year, Chooser wins a million dollars. But if Chooser thinks about any basketball player before naming the team, Chooser will be cast into the fires of Hell for all of eternity. What should Chooser do? 

I think Chooser should say the first team name that comes into their head, or just pass and get out of the game if that's possible. They should very much not try to make the choice that maximises expected utility. That will require forming probabilistic judgments about the likelihood of different teams winning, and that will involve thinking about the players on the team, and that will involve damnation. Just say something and hope for the best.

Is this a counterexample to the claim that in non-Demonic cases Chooser should maximise expected utility? No, because that claim is about ideal rationality. When there are constraints on how Chooser can make the choice, ideal rationality doesn't apply. Chooser should be judged by standards of non-ideal rationality, and non-ideal rationality says to say the first team name that comes into his head. I'll make all this a fair bit more precise in chapter \@ref(dilemma), including coming back to whether it makes sense to talk as if there is a single standard of non-ideal rationality. But I hope it's reasonably clear that something like this is the right thing to say. There are multiple ways we can judge individual choices in individual cases. The standards of ideal rationality are one of those ways to judge cases, but not the only way. And sometimes when we ask what someone should do, we are (implicitly) using one of those other ways. I think most purported counterexamples to one or other decision theory in the literature are no more convincing than using **Quick Basketball Choice** to refute expected utility theory. This is especially true when they involve, as this case does, constraints not just on what decision is made, but how it is made. (For example, when they involve the decision maker being punished for randomising). And this is one reason why I distrust arguments from particular examples.

The second reason is more internal to decision theory. The project I'm engaged in, and the project that most philosophers who write about demonic decision theory are engaged in, is the project of generalising expected utility theory to cover Demonic cases. But the methodology of starting with judgments about individual decisions and finding the theory that best fits them does not, in fact, lead to expected utility theory. So that methodology is inconsistent with the theory we are assuming to be correct in non-Demonic cases. So there is a fairly deep tension in any work that tries to generalise expected utility theory by appeal to intuitions about cases. This point requires a bit of background to make, so let's come back to it after saying more precisely what the theory that we're all trying to generalise is.

# Decision Tables and Decision Problems {#tables}

Five questions

1. What is a permissible state?
2. When is it permissible to end the list of states?
3. What is a permissible option?
4. When is it permissible to end the list of options?
5. What do the values measure?

And a sixth question, what does it take to individuate a problem.

# Non-Demonic Decision Theory {#quiggin}


# Backward Induction {#backwardinduction}

What I'm using
Joyce's review of Ahmed for why it matters
The strong version I'm not using (or maybe that I am using)
Stalnaker on why it shouldn't be used