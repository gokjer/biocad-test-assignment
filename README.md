# biocad

Структура модулей
-----------------
- Main -- подключение к базе и запуск
- Types -- Основные типы данных
- Database -- Основной модуль. Общение с базой: чтение и запись
- Database.Fill -- модуль исключительно для первичного наполнения базы. База заполнена частично реальными реакциями, частично порождёнными для демонстрации

Компоненты
-----
Реализованы согласно описанию. Добавлен компонент `REAGENT_IN`, имеющий, как и `PRODUCT_FROM`, единственное поле `amount :: Float`. Из задания не было ясно, является ли поле `id` идентификатором объекта в базе или отдельной сущностью для каждого типа компонент. Я предполагал последнее, а так же уникальность этого поля в рамках одного типа. Можно несложно исправить на противоположный вариант.
Также хотелось отметить, что спецификация предполагала наличие поля `id` сразу у нескольких разных типов данных. Это вызывает сложности с тем, что необходимо использовать DuplicateRecordFields, а так же во многих случаях явно указывать тип `id`, что неудобно. Можно переименовать в отдельные поля для каждого типа. Например: `rid`, `mid`, `cid`.

Требуемый функционал
----

##### создайте соответствующие типы в haskell-библиотеке
Типы созданы в модуле Types.
##### напишите функцию, которая умеет принимать реацию на вход и загружать её в базу
Функция `Database.addReaction` принимает на вход haskell-объект реакции, а также списки всех остальных составляющих (реагенты, продукты, катализаторы) и загружает в базу ту их часть, которая отсутствует в базе. Также в модуле есть функция `putReaction` (и её аналоги для других типов), которая просто добавляет в базу объект реакции без связей.
##### напишите функцию, которая по номеру реакции в базе будет возвращать её в haskell-объект
Функция `Database.getReaction` решает данную задачу. Также есть соответствующие аналоги для других объектов и отношений.
##### напишите функцию, которая по двум заданным молекулам ищет путь через реакции и молекулы с наименьшей длиной
Функция `Database.findPath` решает данную задачу.
##### подумайте, как поддерживать функциональные зависимости между записями, упомянутые в предыдущем разделе.
См. ниже про функциональные зависимости.

Размышления и замечания
-------
##### Функциональные зависимости
Функциональные зависимости бывают в классах типов между аргументами классов. Например, 
```
class Dependent foo bar | foo -> bar where
    fun :: foo -> bar
```
В этом контексте не очень понятно, что может пониматься под функциональной зависимостью двух полей записи конкретного типа `String`. Функциональные зависимости могут появиться в следующих обобщениях:
- Превратить `Molecule` и остальные типы в классы типов. В общем виде что-то наподобие `class Molecule idType smilesType iupacNameType`. Тогда вполне может быть функциональная зависимость между `smilesType` и `iupacNameType`. Но для оценки необходимости такой конструкции нужно лучше представлять себе задачи и требования к полям `smiles`, `iupacName` и другим.
- Конкретно в моей реализации у функции `addReaction` есть аргументы типов `[(Catalyst, ACCELERATE)]`, `[(Molecule, REAGENT_IN)]`, `[(Molecule, PRODUCT_FROM)]`. Легко заметить, что тип первого элемента пары во всех трёх списках полностью определяется типом второго. Можно обернуть эти значения в один класс и получить функциональную зависимость. В данный момент в этом не ощущается необходимости, но реализовать не сложно.

##### Побочные продукты реакции
Как правило, в химической реакции получается больше одного продукта, а интересует один. Например, при фотосинтезе выделяется кислород, но интересует нас скорее глюкоза. В решении задачи синтеза веществ, скорее всего, не имеет смысла хранить все побочные продукты в форме `Molecule`: этот тип отражает интересные нам молекулы. Однако, хранить информацию о побочных продуктах (или эффектах) реакции может быть полезно. Я вижу два варианта: хранить эти данные в самом `Reaction` или выносить в объект `ReactionResults`. Второй вариант кажется предпочтительнее, дабы не перегружать часто используемый `Reaction`.

###### Логика катализаторов
Насколько я знаю, в реакции может быть несколько катализаторов (их смесь). При этом условия (в постановке -- температура и давление) в основном одни. В таком контексте не совсем правильным кажется то, что условия реакции указываются для каждого катализатора отдельно, хотя они общие. Можно привязывать к `Reaction` объект `Acceleration` с условиями катализа, а к этому объекту уже привязывать `Catalyst`'ы.

##### Вариативность реакций

Одна и та же реакция может проистекать в разных условиях и даже с разными реагентами. Пример, отраженный в коде: обычный и бескислородный фотосинтезы, которые оба приводят к получению глюкозы из углекислого газа. Есть более явные примеры, когда для реакции подходит почти любая кислота. Соответственно, имеет смысл думать о том, как группировать по сути одинаковые либо похожие реакции так, чтобы можно было с ними работать. Это можно решить, например, вводом объекта `ReactionClass`, к которому будут вести связи от конкретных реакций и который будет содержать существенную информацию о реакции.
Отдельный вопрос -- про разные условия (разные катализаторы, разная среда). В данном случае можно привязывать к `Reaction` несколько разных `Acceleration`/`Catalyst`, имея в виду, что подходит любой. Или же можно создавать несколько отдельных `Reaction`.  Первый вариант кажется более правильным и менее ресурсозатратным.

##### Text vs String
В Хаскель-сообществе сложился консенсус о том, что `Text` для использования предпочтительнее, чем `String`. Многие компании (например, Serokell) заменяют `String` на `Text` в `Prelude`. Библиотека hasbolt, которую, видимо, предполагалось использовать для подключения к neo4j, использует `Text`. В данном контексте кажется разумным и в типах данных использовать `Text`, а не `String`.


##### Тип `Reaction`
Сам по себе тип `Reaction` не несёт в себе никакой информации: суть заключается в связях. Кажется, что было бы правильным привязывать структуру этих связей к хаскель-объекту. К сожалению, язык не предоставляет простых и удобных способов это осуществить.