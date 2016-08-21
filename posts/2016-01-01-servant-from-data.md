----
title: Deriving a Servant Schema from your Data
----

This post assumes some level of familiarity with the “modern Haskell
extension zoo” in particular `DataKinds`, `PolyKinds` and `TypeFamilies`.

## Basic setup

The scenario we are in is a bunch of static data that determines
which routes are valid and which aren’t. I got the idea for this post
while working on documentation for
[haskell-ide-engine](https://github.com/haskell/haskell-ide-engine.git)
using
[servant-swagger](https://hackage.haskell.org/package/servant-swagger). I
simplify the code to make it independent of
`hie`. So `haskell-ide-engine` has a list of plugins each having a
list of commands. You can then make requests to `/plugin/command`
passing all additional parameters via a JSON object. Here a Command
consists of a name and a response that we send back when we get a
request. Let’s take a look at the types

```haskell
data Command =
  Command {cmdName :: T.Text, response :: T.Text}
data Plugin = Plugin { cmds :: [Command]}
type Plugins = M.Map T.Text Plugin
```

The static data (it’s important that it’s static) looks as follows

```haskell
plugin1 :: Plugin
plugin1 =
  Plugin {cmds =
            [Command "cmd1.1" "cmd1.1 response"
            ,Command "cmd1.2" "cmd1.2 response"]}

plugin2 :: Plugin
plugin2 =
  Plugin {cmds =
            [Command "cmd2.1" "cmd2.1 response"
            ,Command "cmd2.2" "cmd2.2 response"]}

pluginList :: Plugins
pluginList = M.fromList [("plugin1",plugin1),("plugin2",plugin2)]
```

Now we take a look at the corresponding servant schema and the handlers

```haskell
type CommandName = T.Text
type PluginName = T.Text
type Param = T.Text
type ParamMap = M.Map T.Text T.Text
type API = Capture "plugin" PluginName :>
           Capture "command" CommandName :>
           ReqBody '[JSON] ParamMap :>
           Post '[JSON] T.Text

lookupCommandResponse :: CommandName -> [Command] -> Maybe T.Text
lookupCommandResponse name =
  fmap response . find (\(Command name' _) -> name == name')

server :: Server API
server plugin command params =
  case lookupCommandResponse command . cmds =<<
             M.lookup plugin pluginList of
    Nothing -> left err404
    Just r -> pure r
```

Nothing fancy going on here, we have a single route, which captures the
plugin and the command name and extracts a map of parameters from the
request body. We won’t use that map here. It’s just there to show how
this can be extended to something useful. Once we have the names we
just do a lookup returning the response if it was successful or a 404 otherwise.

## The Problem

Obviously, the above approach works just fine but there is (at least)
one problem: Even though we know all plugins and commands at compile
time, we don’t tell servant about them. At a first glance this might
not be so bad, but if you want to generate documentation or
client bindings for that API, using something like `servant-swagger`
this is pretty bad. The documentation you can generate from a single
route with two parameters is less useful than it needs to be. Wouldn’t
it be great if we could teach servant about the existing plugins and
commands and thereby profit a lot more from the cool documentation and
binding generation servant provides?

## Generating the Schema

Since the servant API is defined at the type level, we need to move the
names to the type level too. Luckily GHC provides the `GHC.TypeLits`
module for type level strings, and we can also reflect them back to the
value level. So let’s make a type level representation of plugin

```haskell
data PluginText = PluginText Symbol [Symbol]
```

`Symbol` is the equivalent of `String` at the type level. Using
`DataKinds` we get a `PluginType` kind and a `'PluginType` type constructor.
Now we need to create a valid servant schema from a list of these.
For that, we need to do induction on type level lists, so it’s nice to
have a base case, which we’ll call `Fail` for `:<|>`, that always fails.
This base case or identity gives us some sort of
monoid structure with `:<|>` being a type level `mappend` and `Fail`
being `mempty`. Note that `:<|>` is not strictly associative since `(a :<|>
b) :<|> c` is a different type than `a :<|> (b :<|> c)`, but that
doesn’t make a difference in our case.

```haskell
data Fail = Fail

instance HasServer Fail where
  type ServerT Fail m = Fail
  route _ _ _ f = f (failWith NotFound)
```

There is nothing that interesting going on, just note that we have to
fill in `Fail` on the value level for `Fail` on the type level.
Equipped with the identity for `:<|>`, we may move on.
Given a command as a symbol, we just use a type synonym to create a
route for it

```haskell
type CommandRoute cmd = cmd :> ReqBody '[JSON] ParamMap :>
  Post '[JSON] T.Text
```

So what do we do if we have a list of command names? On the value
level, we just create a function and recurse on the
list. Luckily we have functions on the type level called
`TypeFamilies` so let’s use that:

```haskell
type family CommandRoutes list where
  CommandRoutes '[] = Fail
  CommandRoutes (cmd ': cmds) = CommandRoute cmd :<|>
                                CommandRoutes cmds
```

Now that we can route a list of commands, we’ll think about how the
schema for a plugin should look. Let’s assume we already have the route for all
the commands. Now it’s simply a case of prepending the plugin name:

```haskell
type PluginRoute plugin cmdRoutes = plugin :> cmdRoutes
```

So finally, let’s convert a list of `PluginType`s to a servant
schema. We already have all the building blocks, so it’s fairly easy:

```haskell
type family PluginRoutes list where
  PluginRoutes ('PluginType name cmds ': xs)
     = (PluginRoute name (CommandRoutes cmds)) :<|> PluginRoutes xs
  PluginRoutes '[] = Fail
```

## Generating the Servant Handlers

So now we know how to get to the servant schema, but we also need the
handlers that deal with the commands. How can we get from a type level
list of `PluginType`s to an implementation? Type classes! We just do
induction on the lists using (the value level) `Fail`
as the base case and combining the cases using (the value level)
`:<|>`:

```haskell

class HieServer (list :: [PluginType])  where
  hieServer
    :: Proxy list -> Server (PluginRoutes list)

instance HieServer '[] where
  hieServer _ = Fail

instance (KnownSymbol plugin,CommandServer cmds,HieServer xs)
          => HieServer ('PluginType plugin cmds ': xs) where
  hieServer _ =
    pluginHandler :<|> hieServer (Proxy :: Proxy xs)
    where pluginHandler
            :: Server (PluginRoute plugin (CommandRoutes cmds))
          pluginHandler =
            cmdServer (T.pack $ symbolVal (Proxy :: Proxy plugin))
                      (Proxy :: Proxy cmds)

class CommandServer (list :: [Symbol])  where
  cmdServer
    :: T.Text -> Proxy list -> Server (CommandRoutes list)

instance CommandServer '[] where
  cmdServer _ _ = Fail

instance (KnownSymbol x,CommandServer xs)
  => CommandServer (x ': xs) where
  cmdServer plugin _ =
    cmdHandler plugin
               (Proxy :: Proxy x) :<|>
    (cmdServer plugin (Proxy :: Proxy xs))

cmdHandler
  :: KnownSymbol x => T.Text -> Proxy x -> Server (CommandRoute x)
cmdHandler plugin cmd reqVal =
  case lookupCommandResponse cmd' . cmds =<<
             M.lookup plugin pluginList of
    Nothing -> left err404
    Just r -> pure r
    where cmd' = T.pack $ symbolVal cmd
```

### Moving command and plugin names to the type level

We want to preserve the data representation we have right now since
there might be a lot of code that uses it and shoving around stuff
with complicated types is often not trivial, e.g. you need to hide
arguments in an existential to put it in a map. It would be great if
we could just tag our existing `Command` type with a `Symbol`. That’s
exactly what `Const` is for. There is a small problem here: `Const` in
GHC 7.10 is not polykinded, so we can’t use a `Symbol` here (in GHC 8.0
it will be polykinded). Luckily
[vinyl](https://hackage.haskell.org/package/vinyl) provides a
polykinded `Const` in `Data.Vinyl.Functor`. Let’s build a function to
create a tagged command:

```haskell
buildCommand
  :: KnownSymbol s
  => Proxy s -> T.Text -> Vinyl.Const Command s
buildCommand name response =
  Vinyl.Const (Command (T.pack $ symbolVal name) response)
```

We use the `KnownSymbol` type class to reflect the string back to the
value level. The `Proxy` here is not actually needed, but I found it
more intuitive to specify the type in the arguments.
Now we have a slight problem: we no longer have a list of `Commands`
but a list of `Vinyl.Const Command s` with the s being different for
every `Command`. Since the standard haskell list is uniform, we can’t
use that anymore. Again Vinyl saves us by providing a
[Rec type](https://hackage.haskell.org/package/vinyl-0.5.1/docs/Data-Vinyl-Core.html#t:Rec),
which takes data that varies in the last type parameter and keeps
track of those parameters in a type level list. Since we want to
preserve the original representation we pull out the type of the
commands giving us

```haskell
data Plugin cmds = Plugin { cmds :: cmds }

type UntaggedPlugin = Plugin [Command]
type TaggedPlugin cmds = Plugin (Vinyl.Rec (Vinyl.Const Command)
                                           cmds)
```

We need to slightly change our data

```haskell
plugin1 :: TaggedPlugin '["cmd1.1","cmd1.2"]
plugin1 = Plugin (buildCommand (Proxy :: Proxy "cmd1.1")
                               "cmd1.1 response"
         Vinyl.:& buildCommand (Proxy :: Proxy "cmd1.2")
                               "cmd1.2 response"
         Vinyl.:& Vinyl.RNil)
```

We still don’t have the plugin name.
Let’s see where we want to go and work our way backwards from there:

```haskell
taggedPlugins :: Vinyl.Rec (Vinyl.Const (T.Text,UntaggedPlugin))
                 '[ 'PluginType "plugin1" _
                  , 'PluginType "plugin2" _]
taggedPlugins = tag plugin1 Vinyl.:& tag plugin2
                            Vinyl.:& Vinyl.RNil
```

The underscores represent the list of command names. You can either
write them here manually or use `PartialTypeSignatures` to let GHC
infer them for you if you are lazy like me.
Once we have this type, we can use `Vinyl.recordToList` to get our
original value level representation:

```haskell
pluginList :: Plugins
pluginList = M.fromList $ Vinyl.recordToList taggedPlugins
```

So what should tag do? We’re going to define that in two steps: first we
wrap it in another layer of `Const`, this time adding the plugin
name. Then we smash them together, giving us a `PluginType` type parameter.

```haskell
untagPlugin :: TaggedPlugin cmds -> UntaggedPlugin
untagPlugin (Plugin cmds) = Plugin $ Vinyl.recordToList cmds

retagPlugin
  :: forall name cmds.
     KnownSymbol name
  => Vinyl.Const (TaggedPlugin cmds) name
  -> Vinyl.Const (T.Text,UntaggedPlugin)
                 ('PluginType name cmds)
retagPlugin (Vinyl.Const desc) =
  Vinyl.Const $
  (T.pack $ symbolVal (Proxy :: Proxy name),untagPlugin desc)

type NamedPlugin name cmds = Vinyl.Const UntaggedPlugin
                                         ('PluginType name cmds)

tag
  :: KnownSymbol name
  => TaggedPlugin cmds
  -> Vinyl.Const (T.Text,UntaggedPlugin) ('PluginType name cmds)
tag = retagPlugin . Vinyl.Const
```

Hold tight we’re almost done! All that’s left is to throw away the
data from the `Rec` type and make a `Proxy` out of it.

```haskell
recProxy :: Vinyl.Rec f t -> Proxy t
recProxy _ = Proxy
```

So finally we can serve our API

```haskell
serveAPI :: forall plugins.
            (HieServer plugins,HasServer (PluginRoutes plugins))
         => Proxy plugins -> IO ()
serveAPI plugins = run 8080 $ serve
  (Proxy :: Proxy (PluginRoutes plugins)) (hieServer plugins)

servePlugins :: IO ()
servePlugins = serveAPI (recProxy taggedPlugins)
```

### Conclusion

To profit from servant’s full potential, you need to move as much
information as possible into your API declaration. It might look like
a fair amount of work, but considering you now get documentation &
client bindings that might actually be useful, I think it’s worth a
trouble (also it’s a lot of fun :)).

You can find the full code on
[github](https://gist.github.com/cocreature/86702eae354f37f0ed8a).

If you are interested, the PR adding this to `haskell-ide-engine` can
be found [here](https://github.com/haskell/haskell-ide-engine/pull/152).
