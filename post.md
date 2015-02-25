## 1 Introduction

[Haskell](http://www.haskell.org/haskellwiki/Haskell) is magic and it is not as difficult to program as you may think. You can be productive very early if you use a high-level library for your domain problem. 

There are two programming levels in industrial Haskell: one in which programmers create Embedded Domain Specific Languages (EDSLs), and one in which programmers simply use the EDSLs without much concern about what is below. A mature and efficient development ecosystem needs both levels; you shouldn't need to know about compiler theory to use a compiler.

This tutorial assumes that you have no knowledge of Haskell, but you are interested. You are probably following a basic course like [learn you a Haskell](http://learnyouahaskell.com/), but you have no time and you need to be able to develop browser applications from the beginning.  

There have been exciting developments recently regarding using Haskell in the browser thanks to some Haskell-to-JavaScript compilers like  [fay](https://github.com/faylang), [ghcjs](https://github.com/ghcjs/ghcjs) and [haste](http://haste-lang.org/). The [elm](http://elm-lang.org/) project is a functional language, developed in Haskell, that compiles to JavaScript. 

So there is a lot of interest and effort put into using Haskell for the client side. Browser programming is probably the easiest and most casual way to enter the world of Haskell, and one of the most promising ways to enter into the industry. A program running in the browser can be distributed and executed in any place and on any device, without the need for anything else. A Haskell development environment that runs on the web is immediately accessible to anyone.

There are also a lot of programmers investing many hours of effort trying to overcome the main problems of client-side development: 

* How to avoid the explosive complexity of the callback programming model.
* How to easily create dynamic layouts by taming the HTML DOM tree.
* How to overcome the dynamic typing of the JavaScript language that makes decent-size projects very hard to develop and maintain.

Haskell has a lot to contribute these issues. For example, the Haskell type system is the best tool for debugging, integration and factorization. 

Integration and factorization in Haskell is like plugging connectors into appliances: you know that if the socket does not accept the plug, it is not in the right place or you need an adapter. 

Writing Haskell is like sculpting in stone. Once the program compiles, it works; especially if the program is coded in a high-level EDSL. 

When it comes to callback hell, the user can program imperative statements; Haskell will generate callbacks under the hood. That is what the *[hplayground](http://github.com/agocorona/hplayground)* library does. It also manages form inputs and events within this imperative-like style. 

These events can perform modifications on the HTML DOM using a high-level library that makes writing new DOM elements on the fly as easy as writing  plain HTML code. This is the work done by the  *[perch](http://github.com/agocorona/haste-perch)* library.

In this tutorial, we will use the *Haste* compiler plus some libraries that bring a high-level EDSL for the creation of client-side web applications. We will illustrate how to do it by creating a personal finance application.

The complete code and application is running at [http://tryplayg.herokuapp.com/try/mybudget.hs/edit](http://tryplayg.herokuapp.com/try/mybudget.hs/edit)

## 2 Installation

*Haste* works with the *Glorious Glasgow Haskell Compiler* (ghc), so you have to install *ghc*, *haste* -- the haskell-to-javascript compiler -- and the two libraries: *perch* and  *hplayground*. The instructions can be found at the end of the README for the *hplayground* library [here](https://github.com/agocorona/*hplayground*/blob/master/README.md)

But you don't have to do all that to have a glimpse of how it works; [here](http://tryplayg.herokuapp.com) you have the development environment online. Note that it is for testing purposes, and there is no user authentication at this moment.

*Haste* can translate almost anything *ghc* compiles to JavaScript + HTML. At the Haste home page you can find examples of how to use it to produce JavaScript or HTML with JS embedded. 

## 3 The application

The example program is a simple personal accounting utility. 

![Application](http://i.imgur.com/8UJq1sV.png)

### 3.1 Data definitions 

First, we define the basic account register that we will use:

<!--code lang=haskell linenums=true-->

    data EntryType= Travel 
                  | Food 
                  | Entertain 
                  | Other 
                  | Income 
                  | AllEntries
                      deriving (Read,Show,Eq, Typeable)

    data Entry= Entry{day, month, year :: Int
                 ,description :: String
                 ,amount :: Double
                 ,etype :: EntryType} 
                      deriving (Read,Show)


As you can see, the register has a date, a description, the amount of money spent, and a type which may be `Travel`, `Food`, `Entertain`, `Other` or  `Income`. `AllEntries` is a wildcard for the application logic. 

The name on the left side is the type of the data. On the right side there are the type constructors. In the case of `EntryType`, there are five constructors with no parameters. In the `Entry` data type, there is one constructor which has six parameters.

The `deriving` clause automatically creates instances for the classes `Read`, `Show` and `Typeable` that are required. `Show` is the class that allows writing the register to a string and `Read` reads this string and produces a register.

To store that register in HTML5 localStorage, we need to convert it to JSON. We use the class `Serialize`, defined in the *Haste.JSON* library of the *haste* compiler. We will use a straightforward conversion using read and show instances:

<!--code lang=haskell linenums=true-->

    instance Serialize Entry where
      toJSON= Str . toJSString . show
      parseJSON (Str jss)=  return . read  $ fromJSStr jss

This is not the best way. JavaScript can not read it since we serialize and deserialize the string produced by `show`, which is a serialized Haskell expression, not a proper JSON register. But, hey, we are just starting! There is no need to define any other class or instance. 

The `*JSString` calls convert from JavaScript  strings to Haskell strings and vice-versa.

And now the options that will be implemented:

<!--code lang=haskell linenums=true-->

    data MainOps= Edit | Detail | Preview 

That is, we will add/delete entries, we will query them, and we will plan future expenses.

### 3.2 Main

And here is the main procedure:

<!--code lang=haskell linenums=true-->

    main= do
      addHeader googleGraph

      runBody $ do
        wraw $ h1  "Personal Budget"  <> hr
  
        r <- wbutton Edit "Edit" <|>
             wbutton Detail "View Entries" <|> 
             wbutton Prev "Preview expenses" 
        
        case r of
             Edit -> edit
             Detail -> viewEntries
             Preview -> preview

        return ()

In the first line we tell the application to insert a script reference in the `<head>` element to load the Google Graph API with the chart module. We will need it to show charts.

This is an example of the interaction of Haskell and JavaScript using *haste*. We will see the details of `googleGraph` later.

*hplayground* uses a Haskell library called *perch* that assembles unevaluated Haskell functions instead of text or HTML DOM calls. These functions are called by the rendering engine only if necessary.

`h1` as well as `hr` are *perch* functions, but they are one-to-one with the corresponding HTML tags. They can be concatenated with the `<>` operator which is, let's say, the general Haskell operator for concatenation. It is in a class, *Monoid*, that can be instantiated for each kind of data and also for functions like those provided by *perch*. 

Any well-defined Haskell EDSL uses the same operators, the same syntax, and the same abstractions. You are not learning something specific, but the general Haskell syntax that can be used to orchestrate the application integration of a company or to program a microprocessor. 

`runBody` executes a sequence of Haskell statements that generate DOM elements and attach them to the body of the HTML document -- here is the magic of the Haskell EDSLs.  

A well-defined embedded DSL in Haskell also hides the complexity and heterogeneity of the domain, so that coding is more intuitive.  A pragmatic programmer like me would say that Haskell uses a generalization of imperative coding, in which the DSL designer specifies what to do between one statement and the next. That is specified in the instance of the *Monad* class, which is part of *hplayground* in this case. This monad instance is the `Widget` monad, which creates event handlers in the background.

The first statement of the `do` block adds DOM elements under the body of the HTML document. `wraw` (raw widget) is a call to insert *perch* HTML DOM elements. 

The next block in `main` are the three buttons corresponding to the three main options.  The `<|>` operator mixes two alternative computations and returns the one that returns something valid. When a `wbutton` is pressed, it  will return the first parameter. Later we will see what is inside of `wbutton`.  

Until one of the three buttons is pressed, the case statement is not executed -- since there is no valid response -- and the code is waiting in the three event handlers that the widget monad has set. There is no blocking here. The event handler that is triggered in the three options is the same:  the case statement.

You see that the coding and the execution model is similar to a console application. It's actually more powerful, since there is no blocking and all the widgets in the sequence are active waiting for input. In a console application, the previous input statement are "dead". 

Later you will see that this model is a blend of console, reactive, window oriented and spreadsheet execution at the same time, thanks to Haskell magic!

Let's go for the first option:

<!--code lang=haskell linenums=true-->

    edit=  do
      entries <- getEntries
      let num = length entries :: Int
      wraw $ div ! id "regnumber" $ b num  <> lb " registers"
  
      r <-  wbutton True "new Entry" <|> wbutton False "Remove Last entry"
      case r of
        True -> newEntry 
        False ->  do
            entries <- getEntries
            if null entries then return () else do
                let entries'= tail entries 
                liftIO $ setEntries  entries'
                let num'= num -1
                at "regnumber" Insert $ wraw $ b (num') <> lb " registers"
        
### 3.3 LocalStorage

First, the registers are read from localStorage using the Haskell procedure `getItem` defined in *haste*. Local storage is persistent, so you will have them every time you start the application.

<!--code lang=haskell linenums=true-->

    -- read the entries from Local Storage
    getEntries :: Widget [Entry]
    getEntries= liftIO $  do
        mr <- getItem "budget"
        case mr of
            Left _ -> return []
            Right list -> return list
 

`getEntries` is a *widget*, like `wbutton`. When called it returns a list of `Entries`.

### 3.4 Building DOM Elements

The next line in `edit` is a `wraw` call, which `insert` a `div` tag with an `id` that includes the number of registers retrieved. The DOM tree generated in this sentence is:

<!--code lang=markup linenums=true-->

    <div id="regnumber"><b>7</b><span class="label"> registers</span></div>

That is because `lb` was defined in the application as:

<!--code lang=haskell linenums=true-->

    lb text= span !  class_ "label" $ text

The operator `!` adds attributes to a *perch* DOM element. After `$` comes the content of the element, in this case the word "registers". That is how *perch* assembles builder functions to create a DOM subtree -- you can contemplate and use it as if it were HTML. 

We use the HTML class to assign CSS styles later. At `main` I could set the label style:  

<!--code lang=haskell linenums=true-->

    addHeader $ nelem "style" `child` ".label1 {float: left;width: 20%;}"

That is *perch* code too. `nelem` is a low level primitive that creates a tag, the `<style>` in this case. `child` is another low level primitive that appends a child node (here, a text element with the style definition) to the style element. This is because the *perch* *style* element is not defined, since it collides with the much more-used style attribute.

*CSS* is one more level of indirection that converts web development into torture. I spent a lot of time trying to align everything; my failure was omitting a semicolon somewhere. *This illustrates the main problem of web development: identifiers and syntax peculiarities in different files and formats everywhere that fail silently without any clue.*

Fortunately there are also Haskell libraries (EDSLs) that enforce the creation of correct CSS definitions by means of the Haskell type system. They can be compiled with *haste* and used for this purpose, but we won't do that now; in this example app, I will reduce formatting to the bare minimum.

These CSS styles can be in a separate file and included with `addHeader`, or they can be defined online. 

`$` is also a general Haskell operator. It means 'take what is on the left of me as a function and execute it with the result of the entire right side as the parameter.'

`edit` has two options: one for creating a new Entry and another for deleting the last one. This second option calls `tail` to drop the last addition and then the *hplayground* primitive `at`, then `Insert`, at the just-created 'regnumber' div, the new number of registers. The parameter of `at` is a *Widget*; in this case, the rendering of `wraw`. `at` also can append and prepend content to a DOM element. Later we will see that `at` can be used to separate layout from logic.

`newEntry` is a long procedure. It contains the form and the logic for a new `Entry`, with some new elements. Let's go for it:

<!--code lang=haskell linenums=true-->

    newEntry= do
      let focus= atr "autofocus" "true"
      desc <- br ++> lb "Enter description: " 
                        ++> inputString Nothing ! size "40" `fire` OnChange 
                        <++ br
      amount <- lb "Enter amount: " ++> inputDouble Nothing ! focus  `fire` OnKeyUp <++ br

`inputString` is a passive text box that returns the content, but it has attached an event handler that fires with the *onchange* event. The  *hplayground* monad creates in the background a JavaScript event handler. 

When the user enters content in the input box and presses the enter key, the event handler calls the rest of `newEntry`. But until this happens, nothing more is shown in the page; it is like an  input statement in a console application. This is the default execution model in which *hplayground* operates. But this style can be overridden for a more static, template-based approach that separates layout and logic. I will detail it later.

### 3.5 More operators

The operators `++>` and `<++` are exclusive to *hplayground*. The first prepends HTML *perch* formatting to the `inputString` widget, and the second one appends it. 

The second input statement is similar. In this case it gets a valid double number; if the user enters something that is not a number, the computation will not continue.

This is the HTML DOM generated by the 'Enter description' statement when it is empty waiting for input:

<!--code lang=haskell linenums=true-->

    <span id="p22">
       <br>
        <span class="label">Enter description: </span>
        <input type="text" id="p37" value="" size="40" change="true">
        <br>
       <span id="p36"></span>
    </span>

The empty span *p36* is where the rendering generated by the callback will be located when triggered.

The next thing in the `newEntry` procedure is to ask for the date of the entry:

<!--code lang=haskell linenums=true-->

      day   <- getDay
      month <- getMonth
      year  <- getYear
  
      (day, month,year) <- getDate (day,month,year)
  

`getDate` receives today's date as the parameter, obtained by calling JavaScript. These four functions will be explained later.

### 3.6 Radio buttons

Next in `addEntry` is a set of radio buttons. They have attached `onclick` events:

<!--code lang=haskell linenums=true-->

      typer <- getRadio(
                   [\n -> lb "Travel "++> setRadio  Travel n  `fire` OnClick <++ br
                   ,\n -> lb "Food "  ++> setRadio  Food   n  `fire` OnClick <++ br
                   ,\n -> lb "Entertainment" ++> setRadio  Entertain n `fire` OnClick <++ br
                   ,\n -> lb "Other " ++> setRadio  Other  n  `fire` OnClick <++ br]

                   ,\n -> hr ++> lb "Income " ++> setRadio Income n  `fire` OnClick <++ br])

The radio buttons are in a list. They are what in functional languages are called *lambda expressions*. The parameter of the lambda expression is the common identifier, provided by `getRadio`. This is arguably too complicated and will be simplified soon.

There is a primitive with a nice name, `setRadioActive`, defined as:

<!--code lang=haskell linenums=true-->

    setRadioActive v n= setRadio  v n  `fire` OnClick

But it is more illustrative to see it unfolded. 

### 3.7 How to create an active widget

The code continues with:
  
<!--code lang=haskell linenums=true-->

      let newEntry= Entry day month year desc amount typer
  
      h1 "Click here to confirm" `pass` OnClick 
  
That is something new. First the register is created using the `Entry` constructor, with the results of the previous widgets. 

And there is text with `h1` formatting with an *onclick* event attached. `newEntry` will not continue beyond this point if the text is not clicked.  

`pass` assigns an event handler to a DOM element,  while `fire` assigns it to a widget. `pass` returns the event content, while `fire` returns the widget content.  

`pass` can be used to create active widgets. For example, `wbutton` is defined in the module *Haste.HPlay.View* as:

<!--code lang=haskell linenums=true-->

    -- | active button. When clicked, return the button value
    wbutton :: a -> String -> Widget a
    wbutton x label= do
            input  ! atr "type" "submit" ! atr "value" label `pass` OnClick
            return x

I would have included just another `wbutton` for the confirmation, but with this hint and a little knowledge of HTML and CSS you will probably be able to create nice tab widgets that substitute for the `wbutton` widgets on the three main options above.

The only remaining thing is to add the new entry to localStorage and to update the number of registers in the 'regnumber' div element:

<!--code lang=haskell linenums=true-->

      entries <- getEntries
      liftIO $ setEntries $ newEntry  : entries
      let num= length entries + 1  
  
      wraw $ lb "Registered! "
      at "regnumber" Insert $ wraw $ b num <> lb " registers"

### 3.8 Haskell errors

You can see that the code is very close to the problem and there is virtually no plumbing. The type system allows for experimentation and factorization without care for the details, since it will warn about missing variables, mismatches in the number and type of parameters, improperly located calls, etc.

There is a downside of all of these advantages: while the EDSL, you and me speak in terms of HTML elements, combo boxes, events, textareas, and procedures, the compiler speaks in Haskell.

For example, if we omit `Insert` in the last line on the above snippet we will have two long errors. One of them says nothing interesting; the second error starts with:

<!--code lang=haskell linenums=true-->

    mybudget.hs:71:31:
        Couldn't match type `View *perch* IO ()' with `UpdateMethod'
        Expected type: UpdateMethod
          Actual type: Widget ()
       .....

`UpdateMethod` is the type of `Insert`. If you search for it on Google, go to the *hplayground* documentation [here](https://hackage.haskell.org/package/*hplayground*-0.1.0.4/docs/Haste-HPlay-View.html#v:at) -- you will see it. What the error means is that instead of an `UpdateMethod`, there is a widget; that must be the second parameter, not the first. 

If you work with EDSLs, you'll notice that errors are constrained to a few variations. Once you know what they mean, by looking at the examples, you can figure out what to do. The good thing is that you will not experience the consequences of these errors at runtime. But expect some extra time spent interacting with the compiler.

The query functionality is in the `viewEntries` procedure which has some other interesting things to mention:

<!--code lang=haskell linenums=true-->

    viewEntries :: Widget ()
    viewEntries= do
       wraw $ br <> lb "from:" 
       today@(d,m,y) <- (,,) <$> getDay <*> getMonth <*> getYear
       (dayf,monthf,yearf) <- getDate (d,if m > 1 then m-1 else m, y)
       wraw $ br <> lb "to:"
       (dayt,montht,yeart) <- getDate today
   
       let filter reg=
            let yearr= year reg; monthr = month reg; dayr= day reg
        
            in  yearr > yearf && yearr < yeart ||
                yearr == yearf && monthr > monthf ||
                yearr == yeart && monthr < montht ||
                monthr == monthf && dayr >= dayf ||
                monthr == monthr && dayr <= dayr
 
       detailByFilter filter 

It is a procedure that asks for an initial and final date and presents the entries between the two dates with `detailByFilter`. 

### 3.9 Applicative 

Here is the first example of another way of computation in Haskell: the *applicative* style:

<!--code lang=haskell linenums=true-->

    today@(d,m,y) <- (,,) <$> getDay <*> getMonth <*> getYear

`<$>` and `<*>` are standard Haskell applicative operators. This right expression means 'execute `getDay`, `getMonth` and `getYear` and pack their results in a 3-tuple.'

The left expression is a pattern match. It says that the 3-tuple will be called `today`, with the day assigned to `d`, the month to `m` and the year to `y`. 

The applicative expressions are defined in the applicative class instance. In *hplayground* this instance generates a static layout and the rendering is fully executed. That permits more static widgets.

A more complex example of applicative expression is `getDate`:

<!--code lang=haskell linenums=true-->

    getDate (day, month, year)= 
       (,,)   <$> lb "dd/mm/yyyy" 
             ++> inputInt (Just day)    ! length_ "2" ! size "2"
                   `validate` (\d -> return (if d> 1 && d <31 
                                              	then Nothing else Just $ b "wrong"))
             <*> inputInt (Just month)  ! length_ "2" ! size "2"
                   `validate` (\m -> return (if m>1 && m < 12 
                                                 then Nothing else Just $ b "wrong"))
             <*> inputInt (Just year)   ! length_ "4" ! size "4"
             <** inputSubmit "Ok" `fire` OnClick
             <++ br

This procedure generates the same kind of data: it returns a 3-tuple. It has the `<$> `and two `<*>` operators, but the operands are now input boxes for *Int* values. These `inputInt` use the `validate` procedure, whose meaning is evident. It takes the value entered by the user and executes the expression. 

If the validator has `Nothing` objectionable, the applicative expression return the 3-tuple. Otherwise, the application will continue asking for the date until a valid date is entered.

As you can see, *getDate* uses plain boxes instead of combo boxes. We could have integrated pretty JavaScript libraries that present a beautiful calendar, following the Google Graph integration that will be explained below.

The applicative style computation can be used to generate static layouts. `getDate` also has some formatting, added with the operators `++>` and `<++` already mentioned.  

A new applicative operator `<**` is introduced here. It is specific to *hplayground*, but it is very similar to the standard applicative operator `<*`. Both discard the value of the operand that is after it, but add the rendering.

They differ in one thing: the first is executed whether the left is validated or not, while the second does not execute in the latter case. 

Since applicative forms need to be rendered fully no matter if the value entered is validated or not, the  `<**` operator is necessary. The same could be said for `<**` and `<*` that operates in the other side.

Some words about operator priorities: The absence of parenthesis is due to a careful choice of the priorities. The applicative operators have less priority than 
the HTML formatting operators, so the `++` operators are more sticky. However, to start with the use of operators, enclose the operands in parenthesis if you are not sure. More on operator priorities later.

`detailByFilter` gets the filter function defined in `viewEntries` It uses some standard functional primitives...

<!--code lang=haskell linenums=true-->

    detailByFilter :: (Entry -> Bool) -> Widget ()

    detailByFilter fil  = do
      regs' <- getEntries
      let regs = filter regs'
        filterByType type_ rs= filter (\r -> etype r == type_) rs

        total :: EntryType -> Double
        total typer=sum $ L.map amount $ filterByType typer regs
        
        travel= total Travel
        food=   total Food
        enter=  total Entertain
        other=  total Other
        income= total Income

...like `filter`, `sum` and `map`. They are used to select the registers that meet the criteria and to obtain the total spent by each concept.

What comes next is a alternative between link widgets:

<!--code lang=haskell linenums=true-->
      
     typer <-   lb <<< wlink Income << lb " Income: " <++ b income
            <|> lb <<< wlink Travel << lb " Travel: " <++ b  travel
            <|> lb <<< wlink Food   << lb " Food: " <++ b  food
            <|> lb <<< wlink Entertain  << lb " Entertainment" <++ b  enter
            <|> lb <<< wlink Other  << lb "Other: " <++ b  other
            <|> return AllEntries

I mentioned the alternative `<|>` operator; it returns the result of the first validated widget. In this case, `wlink` renders as a link. It has two parameters. 

Like `wbutton`, when clicked, `wlink` returns the first parameter. The second parameter is the HTML of the link.  The operator `<<` adds the content to a *Widget* or a *perch* container element. It is similar to `$`, but it has more priority. That is necessary in *Applicative* or *Alternative* contexts like this where there are other operators around. `<<` also converts the second operand to HTML if it is not already of this type. 

### 3.10 Operator priorities

The operator `<<<` encloses a widget within a container element (`lb` in this case, defined above in the application).  

Since `<<<` has a low priority, less than `<<` and `<++`, but more than the alternative operator `<|>`, then  ` lb` encloses the rendering of all the lines after it up to the next `<|>`.

This expression is equivalent to:

<!--code lang=haskell linenums=true-->

     typer <-   (lb <<< ((wlink Income << lb " Income: ") <++ (b income)))
            <|> (lb <<< ((wlink Travel << lb " Travel: ") <++ (b  travel)))
            <|> (lb <<< ((wlink Food   << lb " Food: ") <++ (b  food)))
            <|> (lb <<< ((wlink Entertain  << lb " Entertainment") <++ (b  enter)))
            <|> (lb <<< ((wlink Other  << lb "Other: ") <++ (b  other)))
            <|> (return AllEntries)

The parenthesis for the alternative operator are not included to avoid messing you up even more. And it does not matter, since it is associative. 

From lower to higher priority: 

`$` ==> `<$>` `<*>` `**>` `<**`  ==> `<|>` ==> `<<<`  -> `++>` `<++` ==> `<<`

But note the last alternative above: it is simply a return statement. It means that if none of the links is pressed, `AllEntries` will be returned.  So this alternative combination always returns something. That means that the next statement in the do sequence will always be executed.

### 3.11 Using `do`  to assemble  DOM elements

But this is not the end of the statement (below). There is more.  

Next in the expression there is a `do` sequence with three HTML statements. What is that?  It is content in the *perch* monad. How do we know that? Because it uses `<++` to attach to the expression, because it has `*perch*` statements, and because if you include something that is not a *perch* statement in the proper way, the compiler will complain loudly. 

The *perch* monad just inserts the elements in sequence:

<!--code lang=haskell linenums=true-->

            <++ do 
                   hr 
                   lb "Balance: " 
                   b (income - travel - food - enter - other)

So the expression above is equivalent to:

<!--code lang=haskell linenums=true--> 

     hr <> lb ìBalance ì <>  b(incomeÖ. 

The advantage of the `do` notation is that it can better express the tree structure of the HTML DOM when the rendering gets complicated. 

In this application, the `do` notation is used for three monads: `Widget`, `*perch*` and `IO`.

<!--code lang=haskell linenums=true-->

            <** drawIt (("Type", "Spent")
                       ,("Travel",  travel)
                       ,("Food",   food)
                       ,("Entertainment", enter)
                       ,("Other",   other)
                       ,("Income",   income))

                       
    let filtered = if typer == AllEntries then regs else filterByType typer regs
    detail  filtered

Finally the `<**` operator attaches the chart graph. It adds nothing to the result of the expression, but it appends the rendering.  `drawIt` receives the calculated values to create the chart graph. More on the chart graphic later.

Finally, if one of the links is clicked, the registers of this type are filtered and presented with detail. If none of them is clicked, all the entries within the two dates are presented.

Detail creates a static presentation of the entries using the `do` in the *perch* monad:

<!--code lang=haskell linenums=true-->

    detail  registers= wraw $ do
        h3 "Al registers selected:"
    
        div $ do
          lb $ b "Date"
          lb $ b ìTypeî
          lb $ b "Description"
          lb $ b "Amount"
          br

        let formatEntry (Entry day month year desc amount typer)= 
          div $ do
            lb $ show day++"-"++show month++"-"++show year
            lb typer
            lb desc
            lb amount

        mconcat [formatEntry entry | entry <- registers]

Note how using nested do notation in the `*perch*` monad it is possible to program the creation of DOM subtrees without the tedious low level DOM manipulation primitives of JavaScript. 

It also defines `formatEntry` to present a register, then calls it for each register using Haskell list comprehension, then concatenate the entries using `mconcat`. 

### 3.12 Cells

The `preview` option is more spreadsheet-like:

<!--code lang=haskell linenums=true-->

    preview= do
      changed <-  h3 "Preview" 
            ++> h4 "Recalculate the budget according with priorities and present a chart graph"
            ++> lb "Income"   ++> cell Income  <++ br 
            <|> lb "Travel" ++> cell Travel <++ br
            <|> lb "Food"   ++> cell Food <++ br 
            <|> lb "Entertainment" ++>  cell Entertain <++ br
            <|> lb "Other"   ++> cell Other <++ br 
             
The first lines present an alternative list of  cells. `cell` is defined below as:

<!--code lang=haskell linenums=true-->

      cell :: EntryType -> Widget EntryType
      cell t= do
            mk (boxCell (show t) :: Cell Double) Nothing  `fire` OnKeyUp 
            return t

That renders a text box. In this case the default value is `Nothing`; when  a key is hit in the box, the cell returns the expense type that has been modified. 

`mk` and `boxCell` are defined in *Haste.HPlay.Cell*. In essence, a boxCell is a text box which is polymorphic; it can read any kind of data as long as it has a *Read* instance. That's why the signature *(Cell Double)* is necessary to specify what we need.

A cell can also be read with `get` and assigned with the operator `.=` . `mk` renders the cell in the page. `mk` also returns the value of the cell, but this is ignored. The value will be recovered later with `get`.

These methods are used below to recalculate the budget according to priorities: the income is distributed among the expenses. The increase or decrease of other expenses changes the `Entertain` budget to match the `Income`. If `Entertain` is modified, the `Other` expenses are modified accordingly.  It is simplistic and unreal, but works as an example of the cell usage.
  
<!--code lang=haskell linenums=true-->
    
     t <- get $ boxCell "Travel";    f <- get $ boxCell "Food"
     e <- get $ boxCell "Entertain"; o <- get $ boxCell "Other"
     i <- get $ boxCell "Income" :: Widget Double
   
     (i,f,o,t,e) <-case changed of
        Travel    -> let e= i- f- o- t in do boxCell "Entertain" .= e ; return  (i,f,o,t,e) 
        Food      -> let e= i- f- o- t in do boxCell "Entertain" .= e ; return  (i,f,o,t,e) 
        Entertain -> let o= i- f- e- t in do boxCell "Other" .= o ; return  (i,f,o,t,e) 
        Other     -> let e= i- f- o- t in do boxCell "Entertain" .= e ; return  (i,f,o,t,e) 
        Income    -> let e= i- f- o- t in do boxCell "Entertain" .= e ; return  (i,f,o,t,e) 

Actually, *hplayground* has an experimental [solver](http://tryplayg.herokuapp.com/try/spreadsheet.hs/edit) of cell dependencies that works like a spreadsheet. It can solve this problem in a more elegant way, but it is still being tested.

After updating the cells, the result is presented in a chart graph:

<!--code lang=haskell linenums=true-->

     if( t >= 0 && f >= 0 && e >= 0 && o >= 0 && i >= 0) 
       then
        drawIt(("Type", "Spent")
             ,("Travel",  t)
             ,("Food",   f)
             ,("Entertainment", e)
             ,("Other",   o)
             ,("Income",   i))
        else
          wraw $ b "No graphics since some quantity is negative"
    
Since the code is executed when a key is pressed, the  distribution will be recalculated and the chart will be redrawn with each key pressed. This illustrates well the reactive capabilities of *hplayground*. If you like a less reactive behavior, use `Onchange` instead of `OnKeyUp`.

## 3.13 JavaScript integration

And now the low level code that interacts with JavaScript, to illustrate how to use JavaScript modules with Haskell. 

I use the client-side chart graph from Google. The JavaScript snippet that loads and integrates the chart is [explained by Google here](https://developers.google.com/chart/).

`googleGraph` includes the static code necessary for including the library. It is called at `main`. `drawIt` is the chart drawing procedure, which creates the canvas and draws the chart. The latter is called in two different places.  

The Haskell code below includes essentially the same HTML as the Google example, with HTML coded as *perch* statements and JavaScript as a string:

<!--code lang=haskell linenums=true-->

    googleGraph :: *perch*
    googleGraph= do
      script ! atr "type" "text/javascript" ! src "https://www.google.com/jsapi" $ noHtml
      script ! atr "type" "text/javascript" $ do
       "var options;\
       \function init(){\
        \google.load('visualization', '1', {packages:['corechart'],'callback' : drawChart});\
        \function drawChart() {\
          \options = {\
            \title: 'Preview expenses'\
          \};\
        \}}\
       \function waitGoogle(){\
          \if (typeof google !== 'undefined') {init();}\
          \else{window.setTimeout(function(){waitGoogle();}, 10);}}\
       \waitGoogle();"

`googleGraph` includes all the static setup, and is an almost exact copy of the  Google example. You can make it 3D by following the next example in the page. 

This HTML is generated dynamically at `main` and stays there, but it could have been included in the HTML page. Being in the code makes the maintenance more flexible for quick applications; If I want to change the options dynamically, I only have to cut and paste from `googleGraph` to `drawIt`. 

`drawIt` includes the dynamic part rendered on each call. Since it will be called in two different locations, the canvas must be created dynamically:
     
<!--code lang=haskell linenums=true-->

    drawIt dat= do
      wraw $ div ! id "piechart" ! style "width: 900px; height: 500px;" $ do
         lb "Please connect to Internet to download the "
         a ! href "https://google-developers.appspot.com/chart/interactive/docs/gallery/piechart"
           $ "Pie Chart graphics" 
         lb "from Google"
    
      wraw $ liftIO $ drawIt' dat

The canvas is a `div` element. If the canvas is not created due to a connection failure, the text above is displayed. 

`drawIt` creates the canvas and display the chart:

<!--code lang=haskell linenums=true-->

      where
      drawIt'= ffi $ toJSString 
                "(function (data){\
                 \var chart = new google.visualization.PieChart(document.getElementById('piechart'));\
                 \return chart.draw(google.visualization.arrayToDataTable(data), options);})"

`ffi` is a *haste* primitive that marshals Haskell parameters from/to JavaScript.

Haskell tuples, according to the [documentation](https://hackage.haskell.org/package/haste-compiler-0.4.2/docs/Haste-Foreign.html), are converted to JavaScript arrays. By looking at the Google example, that is what we need. 

Finally, today's date is obtained from JavaScript using `ffi` too:

<!--code lang=haskell linenums=true-->

    getDay :: Widget Int
    getDay= liftIO $ ffi $ toJSString "(function(){return new Date().getDate()})"

    getMonth :: Widget Int
    getMonth= liftIO $ ffi $ toJSString "(function(){return new Date().getMonth()+1})"

    getYear :: Widget Int
    getYear= liftIO $ ffi $ toJSString "(function(){return new Date().getFullYear()})"

### 3.14 Separating layout from logic

The layout and the logic can be separated. The HTML can be coded in the HTML template or it can be created online as *perch* sentences. 

Let's take as an example the preview functionality, which has a form with some labels:

<!--code lang=haskell linenums=true-->

     preview= do
      changed <-  h3 "Preview" 
           ++> h4 "Recalculate the budget accoring with priorities and present a chart graph"
           ++> lb "Income"   ++> cell Income  <++ br 
           <|> lb "Travel" ++> cell Travel <++ br
           <|> lb "Food"   ++> cell Food <++ br 
           <|> lb "Entertainment" ++>  cell Entertain <++ br
           <|> lb "Other"   ++> cell Other <++ br 
 
That code mixes HTML layout and text with active cell widgets. This may not be a problem when the layout is light like in this case, but most of the time the layout is complicated. 

Sometimes it may be convenient to use a WYSIWYG tool to create the HTML template. Or, instead of using operators, it may be far more clear to code it in HTML or using *perch* sentences. 

The code below is equivalent; it separates the layout (as *perch* sentences) and the widget code in the `Widget` monad:
 
<!--code lang=haskell linenums=true-->
 
    preview= do             
      wraw $ do  -- layout in the *perch* monad, with span placeholders
            h3 "Preview" 
            h4 "Recalculate the budget according with priorities and present a chart graph"
            lb "Income" <> (span ! id "incomeholder" $ noHtml) <> br
            lb "Travel" <> (span ! id "travelholder" $ noHtml) <> br
            lb "Food"   <> (span ! id "foodholder" $ noHtml )  <> br
            lb "Entertainment"  <> (span ! id "enterholder" $ noHtml)  <> br
            lb "Other"  <> (span ! id "otherholder" $ noHtml)  <> br
            
    -- logic, separated  from formatting
      changed <-  at "incomeholder" Insert (cell Income)
                    <|> at "travelholder" Insert (cell Travel)
                    <|> at "foodholder"   Insert (cell Food)
                    <|> at "enterholder"  Insert (cell Entertain)
                    <|> at "otherholder"  Insert (cell Other)

The `span` placeholders are updated by the logic when executed. If the page is static and it has only this option (this is not the case) then the layout can be created right in the HTML template, and the *perch* code can be omitted. 

### 3.15 Session data

*LocalStorage* is persistent, but sometimes you need to store data that only lives for a session. For example, you may want to store the values in the `preview` option, so you can go to other options back and forth.  For this purpose there is `getSData` and `setSData`. The types are:

<!--code lang=haskell linenums=true-->

    getSData :: Widget a
    setSData :: a -> Widget ()

The second stores data of type `a` . The first looks to see if there is a value of type `a` stored. If there is none, the widget returns invalid and the sequence does not continue. 

Then, can you imagine a way to return a default value?

<!--code lang=haskell linenums=true-->

    giveEverSomething=  getSData <|> return someDefaultValue


## 4 Conclusions

I think that the code is pretty understandable at first sight. This is because the EDSL hides all the plumbing under the intuitive metaphors taken from imperative programming and console applications. There are some other abstractions, such as the alternative operators, that are easy to understand. Others, like the applicative ones, are less common. The *perch* layout is almost one-to-one with HTML.

Although *haste* -- the Haskell-to-JavaScript compiler -- and the libraries used here are pretty new, the type system of Haskell makes them solid and consistent, especially suited for quick in-house developments, substituting for Excel spreadsheets and some window-oriented or console applications. 

AJAX can be used to interact with any web server. It is not explained here, but it is [not so complex](http://hackage.haskell.org/package/haste-compiler-0.4.2/docs/Haste-Ajax.html). Maybe in another article. 

There must be an evolution for the creation of more useful, higher-level widgets. It is easy to create functionalities as complex widgets for each domain. Then they can be combined by means of Haskell operators inside *perch* layouts. 

As I demonstrated with the `drawIt` widget, these new functionalities can  integrate  JavaScript libraries, so Haskell can be used to create the logic and the interaction using already tested JavaScript developments for any kind of browser application.