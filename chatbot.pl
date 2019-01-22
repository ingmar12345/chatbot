% wegwijspiet.pl

:- use_module(library('http/http_client')).
:- use_module(library('http/http_open')).
:- use_module(library('http/http_json')).
:- use_module(library('http/json')).

:- consult([alice]).

% greet
category([
    pattern([syntax(greet,_),star(_)]),
    template([
	random([['hello'],['hi']]),
	'\n\nI am wegwijspiet, \nI can help you to guide you from A to B',
	'\nI can talk about the weather',
	'\nI know a lot about Rotterdam and I can tell you interesting poems written by Jules Deelder',
	'\n\nNOTE: make sure your PC volume is maximum to spread the message!',
	'\n\nFeel free to ask some weird questions'
    ])
]).

% decline answer
category([
    pattern([syntax(decline,_),star(_)]),
    that([star(_)]),
    template(['\n',
	random([
	    ['Drinking game for Prolog enthousiasts\n1 shot for every call of the function atomic_list_concat()\n'],
	    ['Roses are red, you suck.\n'],
	    ['De omgeving van de mens is de medemens.\n']
	])
	     ])
]).

% who is Jules Deelder
category([
    pattern([who,is,jules,deelder,'?']),
    template([
	'That`s me'
    ])
]).


% browse through all shipping related facts
category([
        pattern([tell,about,syntax(shipping,_)]),
        template([
		think(rotterdam(shippinglines,A)),
		think(rotterdam(draft,B)),
		think(rotterdam(facilities,C)),
		'\n',A,'\n',B,'\n',C,'\n'
		])
]).

% browse through all harbor related facts
category([
        pattern([tell,about,syntax(harbor,_)]),
        template([
		think(rotterdam(shippinglines,A)),
		think(rotterdam(draft,B)),
		think(rotterdam(facilities,C)),
	        think(rotterdam(quay,D)),
		think(rotterdam(labor,E)),
	        think(rotterdam(area,F)),
	        think(rotterdam('tank storage',G)),
		'\n',A,'\n',B,'\n',C,'\n',D,'\n',E,'\n',F,'\n',G,'\n'
		])
]).

% browse through all tourism related facts
category([
        pattern([tell,about,syntax(tourism,_)]),
        template([
		think(rotterdam(hotels,A)),
		think(rotterdam(restaurants,B)),
		think(rotterdam(cafes,C)),
		'\n',A,'\n',B,'\n',C,'\n'
		])
]).

% browse through all hotel related facts
category([
        pattern([tell,quantity,syntax(hotel,_)]),
        template([
		think(rotterdam(hotels,A)),
		'\n',A,'\n'
	])
]).

% browse through all restaurant related facts
category([
        pattern([tell,quantity,syntax(restaurant,_)]),
        template([
		think(rotterdam(restaurants,A)),
		'\n',A,'\n'
	])
]).

% browse through all cafe related facts
category([
        pattern([tell,quantity,syntax(cafe,_)]),
        pattern([how,many,syntax(cafe,_)]),
        template([
		think(rotterdam(cafes,A)),
		'\n',A,'\n'
	])
]).


% browse through all soccer related facts
category([
	pattern([tell,about,syntax(soccer,_)]),
	template([think(rotterdam(soccer,A)),
		  '\n',A,'\n'
		 ])
]).

% browse through all harbor area related facts
category([
    pattern([tell,quantity,syntax(area,_)]),
    template([think(rotterdam(area,A)),
	      '\n',A,'\n'
	     ])
]).

% browse through all street area related facts
category([
    pattern([tell,quantity,street,syntax(area,_)]),
    template([think(rotterdam(streets,A)),
	      '\n',A,'\n'
	     ])
]).


% browse through all statistics
category([
    pattern([tell,about,syntax(stats,_)]),
    template([
	think(findall(F,rotterdam(_,F),List)), think(atomic_list_concat(List,'\n',T)), '\n',T,
	'\n\n',
	'Do you want a nice poem?\n'

    ])
]).

% catches remaining facts about rotterdam
category([
	pattern([tell,quantity,star([Subject])]),
        template([think(rotterdam(Subject,Information)),
		  '\n', Information,
		  '\n\n',
		  'Do you want a nice poem?\n'
		 ])
]).


% 3 ways to interpret `tell about` questions
category([
    pattern([star(_),to,know,star(_),about,star(_),star(A),star(_)]),
    pattern([star(_),tell,star(_),about,star(_),star(A),star(_),'?']),
    pattern([star(_),syntax(fact,_),star(_),about,star(_),star(A),star(_),'?']),
    pattern([star(_),know,star(_),about,star(_),star(A),star(_),'?']),
    template([srai([tell,about,A])])
]).

% interpret `quantity` questions
category([
    pattern([star(_),syntax(quantity,_),star(_),star(A),star(_),rotterdam,star(_),'?']),
    template([srai([how,much,A])])
]).

% 4 ways to interpret both `tell about` and `quantity` questions
category([
    pattern([star(_),to,know,star(_),about,star(_),star(A),star(_)]),
    pattern([star(_),tell,star(_),about,star(_),star(A),star(_),'?']),
    pattern([star(_),syntax(fact,_),star(_),about,star(_),star(A),star(_),'?']),
    pattern([star(_),know,star(_),about,star(_),star(A),star(_),'?']),
    pattern([star(_),syntax(quantity,_),star(_),star(A),star(_),rotterdam,star(_),'?']),
    template([srai([tell,quantity,A])])
]).

% random facts about rotterdam rotterdam
category([
    pattern([star(_),random,syntax(fact,_),star(_),'?']),
    template([
	'Here is a random fact about Rotterdam:',
	random([
	    [think(rotterdam(streets,T))],
	    [think(rotterdam(cafes,T))],
	    [think(rotterdam(buildings,T))],
	    [think(rotterdam('tank storage',T))],
	    [think(rotterdam(draft,T))]
	]),
	'\n\n',T,'\n\nDo you want to know another fact?\n'
    ])
]).

% sequel to previous pattern
category([
    pattern([syntax(accept,_)]),
    that(['Here is a random fact about Rotterdam:',star(_)]),
    template([
	random([
	    [think(rotterdam(labor,T))],
	    [think(rotterdam(population,T))],
	    [think(rotterdam(history,T))],
	    [think(rotterdam(quay,T))],
	    [think(rotterdam(waste,T))]
	]),
	'Here is another random fact about Rotterdam:\n\n',
	T,'\n\nDo you also want to read a nice poem about the facts?\n'
    ])
]).

% sequel to previous pattern
category([
    pattern([syntax(accept,_)]),
    that([star(_),'Here is another random fact about Rotterdam:\n\n',star(_)]),
    template([think(groeten_uit_rotterdam(T)), T
    ])
]).

% asking for a fact
category([
    pattern([star(_),a,syntax(fact,_),star(_),'?']),
    template([
	'I only know facts about Rotterdam',
	'\nDo you want to know all my facts?\n'
    ])
]).

% sequel to previous pattern
category([
    pattern([syntax(accept,_)]),
    that([star(_),'I only know facts about Rotterdam',star(_)]),
    template([
	think(findall(F,rotterdam(_,F),List)), think(atomic_list_concat(List,'\n',T)), '\n',T
    ])
]).

% programming joke
category([
    pattern([star(_),joke,'?']),
    template(['A pointer walks into an undefined bar, and the bar tender throws a null pointer exception at them.', 'Do you want a nice poem?'])
]).

% sequel to previous pattern
category([
    pattern([syntax(accept,_)]),
    that([star(_),'Do you want a nice poem?']),
    template([
	random([
	    [think(groeten_uit_rotterdam(T))],
	    [think(stadsgezicht(T))],
	    [think(rotterdamse_kost(T))],
	    [think(zwart(T))],
	    [think(jazz(T))]
	]),
	T,'\n'
    ])
]).


% ---- API stuff ----

% navigation help
category([
    pattern([star(_),from,star(A),to,star(B),'?']),
    template([think(atomic_list_concat(A,'',A1)),think(atomic_list_concat(B,'',B1)),
	      think(route(A1,B1,Route,Length,Time)),think(set_var(destination,B)),
	      %write(B),
	      'It will take ',Time,' to reach your destination. The total distance is',Length,'. Follow the steps below:\n\n',
	      Route,
	      '\n\nDo you want to know the weather at your destination?'
	     ])
]).

% distance help
category([
    pattern([star(_),distance,star(_),between,star(A),and,star(B),'?']),
    pattern([star(_),syntax(quantity,_),syntax(kilometer,_),star(_),between,star(A),and,star(B),'?']),
    template([think(atomic_list_concat(A,'',A1)),think(atomic_list_concat(B,'',B1)),
	      think(route(A1,B1,_,Length,_)),
	      'The distance between',A,'and',B,'is',Length,'.'
	     ])
]).

% advanced navigation help and ask for destination street
category([
    pattern([star(_),go,star(_),somewhere]),
    pattern([star(_),go,to,star(_)]),
    pattern([star(_),navigation,star(_)]),
    template(['I can help you to navigate to your destination',
	      '\n\nWhat is the name of the street where you want to go?'
	     ])
]).

% ask for destination house number and save user input
category([
    pattern([star(Street)]),
    that([star(_),'I can help you to navigate to your destination',star(_)]),
    template([think(atomic_list_concat(Street,'',Street1)),
	      think(set_var(street_to,[Street1])),
	      'What is the house number of your destination?'
	     ])
]).

% ask for destination city and save save user input
category([
    pattern([star(House_nr)]),
    that([star(_),'house number of your destination?']),
    template([think(atomic_list_concat(House_nr,'',House_nr1)),
	      think(set_var(house_nr_to,[House_nr1])),
	      'What is the city of your destination?'
	     ])
]).

% ask for origin street and save user input
category([
    pattern([star(City)]),
    that([star(_),'city of your destination?']),
    template([think(atomic_list_concat(City,'',City1)),
	      think(set_var(destination,City)),
	      think(set_var(city_to,[City1])),
	      'What is the name of your current street?'
	     ])
]).

% ask for origin city and save user input
category([
    pattern([star(Street)]),
    that([star(_),'current street?']),
    template([think(atomic_list_concat(Street,'',Street1)),
	      think(set_var(street_from,[Street1])),
	      'In what city you currently are?'
	     ])
]).

% return instructions
category([
    pattern([star(City_from)]),
    that([star(_),'city you currently are?']),
    template([think(get_var(street_to,[Street_to])),think(get_var(house_nr_to,[House_nr_to])),think(get_var(city_to,[City_to])),
	      think(get_var(street_from,[Street_from])),
	      think(atomic_list_concat([Street_to,House_nr_to,City_to],'+',Destination)),
	      think(atomic_list_concat(City_from,'',City_from1)),
	      think(atomic_list_concat([Street_from,City_from1],'+',Origin)),
	      think(route(Origin,Destination,Route,Distance,Duration)),
	      'SUMMARY:',
	      '\nFrom:',Origin,
	      '\nTo:',Destination,
	      '\nDistance:',Distance,
	      '\nDuration:',Duration,
	      '\nHere are the instructions to drive to your destination:\n\n',
	      Route,
	      '\n\nDo you want to know the weather at your destination?'
	    ])
]).

% offer some additional information about the destination city
category([
    pattern([syntax(accept,_)]),
    that(['It will take',star(_)]),
    that([star(_),'SUMMARY:',star(_)]),
    template([think(get_var(destination,Destination)),
	      think(atomic_list_concat(Destination,' ',Destination1)),
	      think(weather(Destination1,Temp,Wind,Forecast)),
	      'It is',Temp,'degrees celcius at your destination.',
	      '\nThe windspeed is',Wind,'.',
	      '\nThe forecast in',Destination,'sais:',Forecast,'.'
    ])
]).

% weather conversation
category([
    pattern([star(_),weather,star(_),in,star(City),'?']),
    pattern([star(_),weather,star(_),at,star(City),'?']),
    template([think(atomic_list_concat(City,' ',City1)),
	      think(weather(City1,Temp,Wind,Forecast)),
	      'It is',Temp,'degrees celcius today in',City,'.',
	      '\nThe windspeed is',Wind,'.',
	      '\nThe forecast sais:',Forecast,'.'])
]).

% timezone help
category([
    pattern([star(_),timezone,star(_),in,star(Location),'?']),
    pattern([star(_),timezone,star(_),of,star(Location),'?']),
    template([think(atomic_list_concat(Location,'',Location1)),
	      think(timezone(Location1,Timezone)),
	      'The current timezone of',Location,'is',Timezone,'.'])
]).

% ---- end of API stuff ----


% ask for random poem
category([
    pattern([star(_),tell,star(_),random,poem,'?']),
    pattern([star(_),read,star(_),random,poem,star(_)]),
    pattern([star(_),listen,star(_),random,poem,star(_)]),
    pattern([star(_),hear,star(_),random,poem,star(_)]),
    template([
	random([
	    [think(groeten_uit_rotterdam(T))],
	    [think(stadsgezicht(T))],
	    [think(rotterdamse_kost(T))],
	    [think(zwart(T))],
	    [think(jazz(T))]
	]),
	T,'\n'
    ])
]).


% ask for poem
category([
    pattern([star(_),tell,star(_),poem,'?']),
    pattern([star(_),read,star(_),poem,star(_)]),
    pattern([star(_),listen,star(_),poem,star(_)]),
    pattern([star(_),hear,star(_),poem,star(_)]),
    template([
	'Sure! Where do you like to read a poem about?\n',
	'Food\n',
	'Jazz\n',
	'Rotterdam facts\n',
	'Black colour\n',
	'Random stuff\n\n'
    ])
]).

% if food
category([
    pattern([food]),
    that([star(_),'Sure! Where do you like to read a poem about?\n',star(_)]),
    that([star(_),'No more questions?',star(_)]),
    template([
	think(rotterdamse_kost(T)),[T]
    ])
]).

% if jazz
category([
    pattern([jazz]),
    that([star(_),'Sure! Where do you like to read a poem about?\n',star(_)]),
    that([star(_),'No more questions?',star(_)]),
    template([
	think(jazz(T)),[T]
    ])
]).

% if rotterdam facts
category([
    pattern([rotterdam,facts]),
    that([star(_),'Sure! Where do you like to read a poem about?\n',star(_)]),
    that([star(_),'No more questions?',star(_)]),
    template([
	think(groeten_uit_rotterdam(T)),[T]
    ])
]).

% if black colour
category([
    pattern([black,colour]),
    that([star(_),'Sure! Where do you like to read a poem about?\n',star(_)]),
    that([star(_),'No more questions?',star(_)]),
    template([
	think(zwart(T)),[T]
    ])
]).

% if random stuff
category([
    pattern([random,stuff]),
    that([star(_),'Sure! Where do you like to read a poem about?\n',star(_)]),
    that([star(_),'No more questions?',star(_)]),
    template([
	think(stadsgezicht(T)),[T]
    ])
]).

% empty question
category([
    pattern([star(_),dont,know,star(_)]),
    pattern([star(_),no,idea,star(_)]),
    template([
	'No more questions? What about reading a nice poem?\n',
	'Just choose a topic so I can write a poem just for you:\n',
	'Food\n',
	'Jazz\n',
	'Rotterdam facts\n',
	'Black colour\n',
	'Random stuff\n\n'
    ])
]).


% ---- functions ----

get_first([Head|Tail], Head, Tail).

route(From,To,Instructions,Distance,Duration) :-
	format(atom(URL),'https://maps.googleapis.com/maps/api/directions/json?origin=~s&destination=~s&key=???',[From,To]),
	http_get(URL,json(Route),[]),

        member(routes=R,Route),
	member(json(P),R),
	member(legs=L,P),
	member(json(J),L),
	member(distance=json(Dist),J),
	member(text=Distance,Dist),
	member(duration=json(Dur),J),
	member(text=Duration,Dur),

	% find all html_instructions from the steps list
	findall(H, (
		    member(steps=Q,J),
		    member(json(S),Q),
		    member(html_instructions=H,S)
        ),List1),

	% elimate HTML tags and replace them with relevant punctuation
	findall(E5,(
		    member(E,List1),
		    atomic_list_concat(L1,'<b>',E),
		    atomic_list_concat(L1,'',E2),
		    atomic_list_concat(L2,'</b>',E2),
		    atomic_list_concat(L2,'',E3),
		    atomic_list_concat(L3,'<div style=\"font-size:0.9em\">',E3),
		    atomic_list_concat(L3,'.\n NOTE: ',E4),
		    atomic_list_concat(L4,'</div>',E4),
		    atomic_list_concat(L4,'',E5)
        ),List2),

	atomic_list_concat(List2,'\n\n',Instructions).

% determines the latitude and longtitude of a given location
location(Location,LatLong) :-
	format(atom(URL),'https://maps.googleapis.com/maps/api/geocode/json?address=~s&key=???',[Location]),
	http_get(URL,json(Raw),[]),

	member(results=R,Raw),
	member(json(J),R),
	member(geometry=json(V),J),
	member(location=json(L),V),
	member((lat=Lat),L),
	member((lng=Lng),L),
        LatLong = [Lat,Lng].

% determines the time zone of a given location
timezone(Location,TimeZone) :-
	location(Location,LatLong),
	atomic_list_concat(LatLong,',',T),

	format(atom(URL),'https://maps.googleapis.com/maps/api/timezone/json?location=~s&timestamp=1458000000&key=???',[(T)]),

	http_get(URL,json(Raw),[]),
	member(timeZoneName=TimeZone,Raw).


% determines the weather of a given city
weather(City,Temperature,Speed,Forecast) :-
	First = 'http://api.openweathermap.org/data/2.5/find?q=',
	Second =City,
	Third = '&mode=json&appid=b4cf58a66894a114580988cc65d8bdcb',
	atomic_concat(First,Second,Buffer),
	atomic_concat(Buffer,Third,URL),
	http_get(URL,json(Raw),[]),
	member(list=L,Raw),
        member(json(L1),L),

	% temperature
	member(main=json(Main),L1),
	member(temp=Temp,Main),
	Temperature is round(Temp - 273.15),

	% wind speed
        member(wind=json(Wind),L1),
	member(speed=Speed,Wind),

	% brief forecast
        member(weather=Weather,L1),
	member(json(Weather1),Weather),
	member(description=Forecast,Weather1).

% facts about the city of Rotterdam
rotterdam(shippinglines,'Rotterdam has 300 shippinglines.').
rotterdam(draft,'The Rotterdam harbor can maintain ships with a draft up to 65 feet.').
rotterdam(cranes,'The Rotterdam harbor contains 6 container cranes.').
rotterdam(quay,'The quay of the Rotterdam harbor has a total length of 33 thousand meters.').
rotterdam(area,'The Rotterdam harbor has a surface area of 24.888 acres.').
rotterdam(facilities,'The Rotterdam harbor offers facilities for ships up to 225 thousand tons.').
rotterdam(labor,'There are 78 thousand industrial workers working in the Rotterdam harbor.').
rotterdam('tank storage','The Rotterdam harbor has 22 million tons of tank storage.').
rotterdam(hotels,'Rotterdam contains 86 hotels, with 553.488 overnight stays, of which 451.988 by foreigners.').
rotterdam(cafes,'There are 1060 cafes in Rotterdam, of which 32 with a night permit.').
rotterdam(restaurants,'Rotterdam has 204 restaurants').
rotterdam(streets,'Rotterdam has a street surface to be cleaned of 23 million square meters.').
rotterdam(population,'In Rotterdam live 160 million people within a radius of 500 kilometers.').
rotterdam(buildings,'In Rotterdam are towering residential complexes with closed through luxaflexes in combination with 238 thousand houses on both sides of the Nieuwe Maas.').
rotterdam(soccer,'Rotterdam has 3 big footballclubs: Feyenoord, Excelsior and Sparta.').
rotterdam(history,'Rotterdam has city rights since 1340, in addition, there are 77 homeless people after the bombing').
rotterdam(waste,'Rotterdam has 349 tons of waste.').


% mp3 player
play(Poem) :-
	working_directory(WD,WD),
        atomic_list_concat([WD,Poem],'',Path),
	%write(Path),

	% force Windows to use Windows Media Player (useful if not default)
	atomic_list_concat(['"C:/Program Files/Windows Media Player/wmplayer.exe"',Path],' ',Command),
	%write(Command),

	% execute command in Windows
	win_exec(Command,showdefault).

	% execute command in Linux
	%process_create(path(play),[Poem],[stderr(null)]).


% Jules Deelder poem #1
groeten_uit_rotterdam(Text) :-
        play('deelder/groeten_uit_rotterdam.mp3'),

	T = [
	    '\nGROETEN UIT ROTTERDAM\n\n',
	    'Rotterdam is een stad met stadsrechten sinds 1340\n',
	    'Met 1060 cafes waarvan 32 met nachtvergunning\n',
	    'Met een groen-wit-groene vlag en 3 horizontale banen\n',
	    'Met 33 duizend meter kade\n',
	    'Met 160 miljoen mensen binnen een straal van 500 kilometer\n',
	    'Met 300 scheepvaartlijnen\n',
	    'Met 5.568 bejaarden in 62 bejaarden tehuizen\n',
	    'Met 6 containerkranen\n',
	    'Met 213 kleuterscholen\n',
	    'Met de euromast\n',
	    'Met een te reinigen straatoppervlak van 23 miljoen vierkante meter\n',
	    'Met 204 restaurants\n',
	    'Met een verbruik van 600 miljoen kubieke meter aardgas\n',
	    'Met de ahoy hal\n',
	    'Met 22 miljoen ton tankopslag\n',
	    'Met 349 duizend ton afval\n',
	    'Met het groothandelsgebouw\n',
	    'Met 553.488 overnachtingen\n',
	    'Waarvan 451.988 door buitenlanders\n',
	    'Op 4.037 bedden in 86 hotels en/of pensions\n',
	    'Met 77 duizend daklozen na het bombardement\n',
	    'Met 78 duizend industriearbeiders\n',
	    'Met een havengebied van 10.076 hectare\n',
	    'Met faciliteiten voor schepen tot 225 duizend ton\n',
	    'Met een diepgang tot 65 voet\n',
	    'Met de kaap, het kasteel en de kuip\n',
	    'Met een wapenspreuk ‘sterker door strijd’\n',
	    'Met 238 duizend woningen ter weerszijden van de nieuwe maas'
	],

	atomic_list_concat(T,'',Text).


% Jules Deelder poem #2
stadsgezicht(Text) :-
	play('deelder/stadsgezicht.mp3'),

	T = [
	    '\nSTADSGEZICHT\n\n',
	    'Tegenwoordigheid van geest\n\n',
	    'en realisme in t kwadraat\n\n',
	    'vieren onverstoorbaar feest\n\n',
	    'in een opgebroken straat\n\n',
	    'Hoog en spijkerhard de hemel\n\n',
	    'met een blikkerende zon\n\n',
	    'of zwart en laag in wilde wemel\n\n',
	    'langs skeletten van beton\n\n',
	    'Doorheen geloken luxaflexen\n\n',
	    'tórenhoog de wooncomplexen\n\n',
	    'stapelen den einder dicht\n\n',
	    'Posthistorisch vergezicht -\n\n',
	    'Rotterdam gehakt uit marmer\n\n',
	    'kant’lend in het tegenlicht'
	],

	atomic_list_concat(T,'',Text).


% Jules Deelder poem #3
rotterdamse_kost(Text) :-
	play('deelder/rotterdamse_kost.mp3'),

	T = [
	    '\nROTTERDAMSE KOST\n\n',
	    'Waar de natie bietjes eet bikt Rotterdam z’n kroten\n',
	    'Vandaar dat daar de krotenkoker heerst\n',
	    'waar elders in den lande de zakkenwasser prevaleert\n\n\n',
	    'De fricandel is al sinds jaar en dag de favoriete doodsoorzaak in Rotterdam\n',
	    'en dat woue we graag zo houe omdat het anders kankeren wordt\n\n\n',
	    'In proletarisch Rotterdam gold uierboord als godenspijs\n',
	    'waarvoor men met een pannetje bij de slager in de rij stond\n',
	    'maar als slachtafval in feite voor de varkens was bestemd\n\n\n',
	    'Azzie niet vreet ga je dood, wist Opoe Herfst (106) al vóór ze geboren werd\n',
	    'Azzie ’t wel doet óók, dient daar voor de goede orde nog aan toegevoegd\n\n\n',
	    'De kapsalon – zowel de grote als de kleine – komt oorspronkelijk uit Rotterdam\n',
	    'Het is een ultravette hap waar een hond geen brood van lust maar een Turk wel páp\n\n\n',
	    'Gerookt of gebakken valt een Harderwijker best te makken\n',
	    'maar niet in Rotterdam daar is en blijft het een boerelul die moeilijk van z`n poen af kan\n\n\n',
	    'Rotterdamse kost is voor een buitenstaander niet te knagen\n',
	    'en alleen de sterkste magen kunnen hem of haar zonder gevaar voor brandend zuur of darmkatarrh verdragen\n\n',
	    'de Rotterdammer is erop gemaakt en zal hóóguit om de tomatenketchup vragen\n\n\n',
	    'Snert-met-drijfijs\n',
	    'Sterf-op-straat-worst\n\n',
	    'Steak-de-moord\n',
	    'en kapsalon\n\n',
	    'Nazi Goering\n',
	    'Neukpatronen\n\n',
	    'Kroten kaantjes\n',
	    'Uierboord\n\n',
	    'Ziedaar de Rotterdamse keuken met de complimenten van de kok\n',
	    'Da’s die Bolle met die badmuts en dat maffe schortje voor\n\n',
	    'Krijgie dáár de schijterij van ál dat gekook op die tv!\n',
	    'Straks is iedereen hier chef waar niemand van de honger nog ontlasting heeft\n\n\n',
	    'Kanen knagen\n',
	    'hooien grazen\n\n',
	    'schaften makken\n',
	    'pruimen prakken\n\n',
	    'bunkeren bikken\n',
	    'happen slikken\n\n',
	    'haggelen snaaien\n',
	    'lossen en laaien\n\n',
	    'Rotterdammers doen het gaarne zolang het maar géén tafelen wordt\n\n\n',
	    '’Rotterdam is een stad waar gewerkt moet worden\n',
	    'en van vreemde praktijken zijn we hier niet gediend’\n',
	    'merkte de koekenbakker op'
	],

	atomic_list_concat(T,'',Text).


% Jules Deelder poem #4
zwart(Text) :-
	play('deelder/zwart.mp3'),

	T = [
	    '\nWAT ZWART IS, MOET ZWART BLIJVEN\n\n',
	    'In het zwart,\n',
	    'door een gat in de nacht,\n',
	    'aan het heden ontsnapt.\n',
	    'Doorheen de mist der eeuwen.\n',
	    'Over bergen en zeeën.\n',
	    'Tot ver voorbij het eind van zijn Latijn gedreven.'
	],

	atomic_list_concat(T,'',Text).


% Jules Deelder poem #5
jazz(Text) :-
	play('deelder/jazz_is.mp3'),

	T = ['\nJazz is. Jazz leeft. Gebeurt. Beweegt. Jazz neemt. Jazz geeft. Jazz weet. Jazz spreekt. Jazz doet. Jazz laat. Jazz komt. Jazz gaat. Uniek. Muziek. Van vlees en bloed. Jazz waagt. Jazz wint. Breekt baan. Jazz bonkt. Jazz staat. Jazz valt. Is overal. Ontroert. Verwarmt. Grijpt bij de keel. Jazz knettert. Knalt. Ontketent. Heerst. Jazz heelt. Jazz zuivert. Lichaam. Geest. Jazz swingt. Jazz vecht. Is waar. Is echt. Geen loze kreet. Geen leeg gebaar. Jazz werkt. Versterkt. Ontwapent. Toont. Jazz laaft. Jazz loont. Is water. Brood. Jazz lacht. Jazz huilt. Jazz in. Jazz uit. Legt bloot. Daagt uit. Jazz kookt. Jazz bruist. Jazz troost.
Jazz bijt. Jazz bloedt. Heeft schijt. Is zwart. Is wit. Is rood. Niet grijs. Jazz vloekt. Jazz moet. Verbroedert. Zoekt. Jazz vindt. Jazz wijst. Jazz schokt. Jazz eist. Jazz hoog. Jazz laag. Jazz voor. Jazz na. Jazz rookt. Jazz jaagt. Is eigen baas. Vereent. Verzoent. Begeestert. Woedt. Bevrijdt. Bewijst. Begrijpt. Vervoert. Jazz spreidt. Jazz sluit. Bezielt. Verrijkt. Geeft hoop. Verblijdt. Jazz shittert. Glanst. Jazz flitst. Jazz danst. Verhit. Zweept op. Bemint. Verleidt. Jazz roept. Jazz voelt. Jazz groeit. Jazz bloeit. Jazz blaakt. Jazz blijkt. Betovert. Geilt. Jazz ademt. Zweet. Jazz fluistert. Schreeuwt. Ontmaskert. Snijdt. Jazz glijdt.
Jazz sluipt. Jazz slijpt. Jazz spuit. Jazz klinkt. Jazz dwingt. Jazz lonkt. Jazz blinkt. Jazz vraagt. Jazz raakt. Verlost. Verbaast. Viert feest. Verklaart. Is bitter. Zoet. Is hot. Is cool. Jazz ijlt. Vooruit. Voorbij. Ver weg. Dichtbij. Paraat. Bereid. Op weg. Altijd. Jazz was. Jazz is. Jazz blijft.'],
	atomic_list_concat(T,'',Text).


% syntax
greet --> [hello];[hi];[good,morning];[good,afternoon];[good,evening].
shipping --> [ship];[ships];[boat];[boats];[shipping].
harbor --> [port];[harbor];[marina].
tourism --> [horeca];[tourism].
hotel --> [hotel];[hotels];[hostel];[hostels].
cafe --> [cafe];[cafes];[pub];[pubs];[bar];[bars];[club];[clubs];[clubbing];[beer];[drink];[drinks].
restaurant --> [catering];[restaurant];[restaurants];[pizzeria];[food];[dinner];[breakfast];[lunch].
area --> [terrain];[area];[surface];[surface,area];[square,meters];[acres];[acre];[square,meter].
to_be --> [is];[was];[were].
fact --> [fact];[facts].
quantity --> [how,much];[how,many];[amount];[number,of];[how,long];[how,long];[how,heavy];[how,big].
accept --> [yes];[sure];[of,course];[yeah].
decline --> [not,now];[no].
kilometer --> [kilometer];[kilometers];[km].
stats --> [stats];[statistics].
soccer --> [football];[soccer];[football,clubs].
