local descEN = [[This mod gives you ability to create freely underground station with distinct and interconnected parts, and to create freely street entry at any place you like.

This mod requires the following mods to work:
* Shader Enhancement mod
* Underpass mod

To build a station, you need follow three steps
1. Build at least one underground level
2. Build an entry nearby
3. Follow the instruction on the screen to finish the construction

This mod is a total reworked mod of Transport Fever Ultimate Underground Station, with generally the same resource the main new features are:
- It's modular, though not in vanilla mode, you can place different tracks and different platforms as modules, as well as stairs. I didn't choose the vanilla way since in real life, underground station are always regular.
- An interactive interface to guide you finish the composition of the station
- More possible values for radius and track numbers etc. thanks to the new parameter menu
- Use of new station sign text system from the game instead of Livetext mod.

This mod is still in evolution, maybe with some bugs, I will do some updates and bugfixes after the release.

----------------------------------------------------------------------

I am a Chinese, living in Europe. My motherland is suffering the pandemic caused by the "Novel Coronavirus" 2019-nCov, several tens of thousands of people are contaminated in weeks and hundreds of people died. We are fighting against the virus and we believe that we will win. I pay my highest respect to these who are working hard to save the people. I pay also my highest respect to all the people, healthy or ill, who is confining themselves at home to avoid the propagation virus. 

However, we are not virus ourselves, and the virus is not called "Chinese virus", these attacks on Chinese just because of our face or because we have masks on the face -- are not justified.
We are on the same earth, we are on the same boat, if you want to protect yourself and protect others, you should put the mask on, wash your hand frequently, but never attack these innocent people, the virus doesn't hold a nationality, and the violence is not able to kill virus.

Keep fighting Wuhan!
Keep fighting Hubei!
Keep fighting China!
Keep fighting all suffered from Novel Coronavirus!
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
            ["UNDERPASS_WARNING"] = [["Modular underground station" mod requires both "Underpass" mod and "Shader Enhancement" mod, you will not be able to build up station and get correct rendering without either.]],

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
