local descEN = [[This mod gives you ability to create freely underground station with distinct and interconnected parts, and to create freely street entry at any place you like.
To build a station, you need follow three steps

1. Build at least one underground level
2. Build an entry nearby
3. Click on "Underground station assembler" to connect them all!

Some hints:
* Use PageUp/PageDown to adjust the depth of the station
* You can build station without any entry
* If you assemble entries without any underground level, it will turn out be pedestrian under-street passage
* Name any underground level or the entry starting with ## to force the platform sign text, if not the mod will choose a least long name between the elements
* Name the underground level starting with single # and switch on corresponding option in the parameters to override the platform length, radius and track layout:
For example:
#L150R300 -> A station of 150m long and of 300m radius
#L150TTPTT -> A station of 150m long, with two tracks on the both sides and a central platform
#PTTTTP -> A station with platform on two sides, and two transit tracks in central
* The "length" parameter defines the length of the shortest track in the station, if curved.
* After the station is assembled, you can't directly modify it, to modify "Disassemble" it first.

This mod requires the following two mods to work:
* Shader Enhancement Mod
* Livetext

This mod is designed to be easily extended, if you want to add new entry or new internal platform models, please ask me freely I will supply technical support and even update for adaptation.
The interconnection with Ultimate Station is in considering.

Since the successor of Transport Fever has been announced upon the release of the final patch, this mod is probably my last big mod. I have spent too much hours on modding and even played the game too little.
Honestly I hope the new game would be more modder friendly since lot's of modding mechanism is missing in the game, that limited the possibilities...
Anyway Thanks to all who love my mod and Urban Games for much technical support.
]]

local descCN = [[本MOD能够自由地创建具有各个独立部分并且相互连接地铁站，并在你喜欢的任何地方自由创建入口。

要新建一个地下车站，你需要遵循三个步骤

1.建造至少一个地下层
2.在附近建一个入口
3.点击“组装地下车站”将它们连接起来！

细节说明：
* 使用 PageUp / PageDown 调整车站的深度
* 可以建造没有任何入口的车站
* 如果只设置入口而没有地下站厅，那么这些入口会被组装为过街地道
* 以##命名任何地下站厅或者入口将强制站台牌上的文本，否则MOD将选择一个最短的名字显示在站台牌上
* 用#命名一个站厅，并且在菜单中把强制选项的打开，那么可以强制使用#命名中指定的长度、半径和轨道布局（暂时不支持中文）：
例如：
＃L150R300 -> 站台150米长，半径300米
＃L150TTPTT -> 站台150米长，两侧各有两条轨道，以及中央一个岛式站台
#PTTTTP -> 侧式站台，并且中央有两条正线
* 组装完车站后，车站无法直接修改，请先“解散”车站后再进行修改

该MOD需要以下两个MOD的支持
* 着色器增强
* Livetext

由于Transport Fever的下一代已经宣布，我不会再创造新的MOD了，我在MOD上花了太多的时间，以至于没有时间玩这个游戏
老实说，我希望新游戏对MOD更加友好，因为TpF里面没有提供很多机制结果让一些MOD的过程变得很诡异并且操蛋......
无论如何感谢所有喜欢我MOD的人以及Urban Games（不管你们国内的玩家怎么叫他们）提供的技术支持。

* 为了偷懒，本说明部分机翻]]


function data()
    return {
        en = {
            ["name"] = "Ultimate Underground Station",
            ["desc"] = descEN,
        },
        zh_CN = {
            ["name"] = "终极地下车站",
            ["desc"] = descCN,
            ["Override Length"] = "强制长度",
            ["Override Radius"] = "强制半径",
            ["Override Track Layout"] = "强制轨道布局",
            ["Number of tracks"] = "轨道数",
            ["Track Type"] = "轨道类型",
            ["Transit Tracks"] = "正线数",
            ["Position"] = "位置",
            ["Left"] = "左",
            ["Centre"] = "中",
            ["Sides"] = "两边",
            ["Right"] = "右",
            ["Wall"] = "墙面",
            ["Tiles 1"] = "瓷砖1",
            ["Tiles 2"] = "瓷砖2",
            ["Floor Style"] = "地面",
            ["Marble 1"] = "大理石1",
            ["Marble 2"] = "大理石2",
            ["Honeycomb"] = "蜂窝",
            ["Concrete"] = "水泥砖",
            ["Asphalt"] = "沥青",
            ["Radius"] = "半径",
            ["Platform"] = "站台",
            ["Length"] = "长度",
            ["Track"] = "轨道",
            ["Fences"] = "围栏",
            ["Style"] = "风格",
            ["Glass"] = "玻璃",
            ["Normal"] = "栏杆",
            ["Concrete"] = "水泥",
            ["Slope"] = "坡度",
            ["Depth Adjustment"] = "高度微调",
            ["Height"] = "高度",
            ["Width"] = "宽度",
            ["Orientation"] = "指向",
            ["Lefthand"] = "向左",
            ["Straight"] = "直线",
            ["Righthand"] = "向右",
            ["Leftmost Platform"] = "左侧站台",
            ["Central Platform"] = "中央站台",
            ["Rightmost Platform"] = "右侧站台",
            ["No"] = "无",
            ["Yes"] = "有",
            ["Form"] = "形式",
            ["Keep"] = "保持",
            ["Disassemble"] = "解散",
            ["Underground Level"] = "地下站厅",
            ["An underground level for a station, width adjustable platform width and radius."] = "可以调节站台宽度和半径的地下站厅.",
            ["Underground Entry"] = "地下入口",
            ["An underground entry to a station"] = "通往地下车站的入口.",
            ["Underground station assembler"] = "组装下地车站",
            ["Unify all un-conntected underground level and entries near the viewport as an interconnected and unique station."] = "合并所有视野内没有连通的地下站台和入口."
        }
    }
end
