Roll your own IRC bot (2024 version)
====================================

Updated code for the tutorial on the Haskell Wiki, using current versions of
*network* and *time*.

https://wiki.haskell.org/Roll_your_own_IRC_bot (by Don Stewart)

Build
-----

```
# If you use cabal
cabal build

# If you use stack
stack build
```

Execute
-------

```
# Bot versions: bot-1, bot-2, bot-3, bot-4, bot-5

# If you use cabal
cabal exec bot-5

# If you use stack
stack exec bot-5
```

Example transcript
------------------

On IRC:

```
08:29 --> tutbot (~tutbot@XYZ) has joined #tutbot-testing
08:31 <lyxia> !id Hello!
08:31 <tutbot> Hello!
08:31 <lyxia> !uptime
08:31 <tutbot> 1m 52s
08:31 <lyxia> !uptime
08:31 <tutbot> 2m 3s
08:31 <lyxia> !id Stop copying me!
08:31 <tutbot> Stop copying me!
08:31 <lyxia> !quit
08:31 <-- tutbot (~tutbot@XYZ) has quit (Client Quit)
```

Command line output:

```
> NICK tutbot
> USER tutbot 0 * :tutorial bot
> JOIN #tutbot-testing
:iridium.libera.chat NOTICE * :*** Checking Ident

... long welcome message ...

:lyxia!~lyxia@XYZ PRIVMSG #tutbot-testing :!id Hello!
> PRIVMSG #tutbot-testing :Hello!
PING :orwell.freenode.net
> PONG :orwell.freenode.net
:lyxia!~lyxia@XYZ PRIVMSG #tutbot-testing :!uptime
> PRIVMSG #tutbot-testing :1m 52s
:lyxia!~lyxia@XYZ PRIVMSG #tutbot-testing :!uptime
> PRIVMSG #tutbot-testing :2m 3s
:lyxia!~lyxia@XYZ PRIVMSG #tutbot-testing :!id Stop copying me!
> PRIVMSG #tutbot-testing :Stop copying me!
:lyxia!~lyxia@XYZ PRIVMSG #tutbot-testing :!quit
> QUIT :Exiting
```
