local descEN = [[This mod gives you ability to create freely underground station with distinct and interconnected parts, and to create freely street entry at any place you like.

This mod requires the following mods to work:
* Shader Enhancement mod
* Underpass mod

To build a station:
1. Build at least one underground level
2. Build an entry nearby
3. Follow the instruction on the screen to finish the construction

To add new parts to a built station:
1. Place an entry or a new underground level
2. Select the underground station you want to modify
3. Check the checkbox before the built station
4. Follow the instruction on the screen to finish the construction

This mod is a total reworked mod of Transport Fever Ultimate Underground Station, with generally the same resource the main new features are:
- It's modular, though not in vanilla mode, you can place different tracks and different platforms as modules, as well as stairs. I didn't choose the vanilla way since in real life, underground station are always regular.
- An interactive interface to guide you finish the composition of the station
- More possible values for radius and track numbers etc. thanks to the new parameter menu
- Use of new station sign text system from the game instead of Livetext mod.

This mod is still in evolution, maybe with some bugs, I will do some updates and bugfixes after the release.

Changelog:
1.4
- Optimization of polygon calculation algorithm
- Correction of polygon calculation of the ceil near to upstairs
- Correction of inclined walls on sloped platforms
- Correction of UV mapping
- Ability to add new platform levels or entry on finished station (Underpass 1.3)
- Remastered icons
- Improved the way to generate internal connection 
1.3
- Fixed a potential crash bug on game loading
1.2
- Fixed some flickering wall issue
- Fixed tunnel entry issue on unfinished platform
1.1
- Fixed conflict with NL Station mod
- Fixed crashes under non-debug mode
- Corrected wrong door open side
- Added "Platform width" option for the construction menu
- Added station name sign on entry
- Added platform number sign
- Improved terminal number stability over station modification
- Possible to modify station parameter after finish building for case of only one platform level
- Implementation of track upgrade function

----------------------------------------------------------------------

I am Chinese, living in Europe. My motherland is suffering the pandemic caused by the "Novel Coronavirus" 2019-nCov, several tens of thousands of people are contaminated in weeks and hundreds of people died. We are fighting against the virus and we believe that we will win. I pay my highest respect to these who are working hard to save the people. I pay also my highest respect to all the people, healthy or ill, who is confining themselves at home to avoid the propagation of virus. 

However, we are NOT virus ourselves, and the virus is NOT called "Chinese virus", these attacks on Chinese just because of our face or because we have masks on the face -- are not justified.
We are on the same earth, we are on the same boat, if you want to protect yourself and protect others, you should put the mask on, wash your hand frequently, but never attack on innocent people, the virus doesn't hold a nationality, and the violence does not contribute to killing virus.

Keep fighting, Wuhan!
Keep fighting, Hubei!
Keep fighting, China!
Keep fighting, all suffered from Novel Coronavirus!
]]

local descCN = [[本MOD提供建造形式丰富的地下车站的可能。

本MOD需要以下MOD的支持才能正常使用：
* 着色器增强
* 人行地道

请按照以下步骤建造地下车站：
1. 建造至少一个地下站厅
2. 建造至少一个车站入口
3. 按照屏幕上的指示完成车站的建造

如果想在已建造完成的地下车站上添加新的入口和站台层：
1. 选择已经建造好的车站
2. 在地图上放置新的入口和站台层
3. 在建造对话框中勾选已建造好的车站
4. 按照屏幕上的指示完成车站的建造

本MOD是使用原Transport Fever的终极地下车站MOD的资源重新制作，有以下新特性：
- 模块化建造，因为现实中地下车站的结构都比较规整的原因，没有采用原装车站式的模块化，但是玩家可以以模块化的方式布置站台和股道结构，并且以零件的方式在站台上设置楼梯。
- 有一个互动式的窗口指引你完成车站的建造
- 更丰富的半径和站台数量组合
- 使用了新版游戏下的站台文字系统

本MOD目前可能还有很多Bug，有一些功能还在完善中，之后还会陆续有更新完善功能和修正错误。

武汉加油！
愿人类战胜病毒！

更新日志:
1.4
- 优化多边形生成算法
- 纠正了楼梯附近站台天花板的多边形生成错误
- 纠正了坡道站台上的墙面倾斜的错误
- 纠正了一些UV映射
- 增加了在已经完成的车站上新增站厅和入口的功能（需要将过街地道升级为1.3版本）
- 重制了图标
- 改进了内部连接道路的生成方式
1.3
- 修正了一可能的奔溃
1.2
- 修正了一些墙面闪烁的问题
- 修正了在未完成修建的站台上连接轨道会出现隧道入口的问题
1.1
- 修复了和荷兰车站模组的冲突
- 修复了非调试模式下的崩溃
- 修正了错误的开门方向
- 在建造参数菜单中增加了“站台宽度”选项
- 在车站入口处增加了站名牌
- 增加了站台号牌
- 改进了修改车站时的站台编号稳定性
- 恢复了当只有一个站台层时完成建造车站后修改车站参数的能力
- 实现了轨道升级功能
]]

local descTC = [[本MOD提供建造形式豐富的地下車站的可能。

本MOD需要以下MOD的支援才能正常使用：
* 著色器增強
* 人行地道

請按照以下步驟建造地下車站：
1. 建造至少一個地下站廳
2. 建造至少一個車站入口
3. 按照螢幕上的指示完成車站的建造

如果想在已建造完成的地下車站上添加新的入口和月臺層：
1. 選擇已經建造好的車站
2. 在地圖上放置新的入口和月臺層
3. 在建造對話方塊中勾選已建造好的車站
4. 按照螢幕上的指示完成車站的建造

本MOD是使用原Transport Fever的終極地下車站MOD的資源重新製作，有以下新特性：
- 模組化建造，因為現實中地下車站的結構都比較規整的原因，沒有採用原裝車站式的模組化，但是玩家可以以模組化的方式佈置月臺和股道結構，並且以零件的方式在月臺上設置樓梯。
- 有一個互動式的視窗指引你完成車站的建造
- 更豐富的半徑和月臺數量組合
- 使用了新版遊戲下的月臺文字系統

本MOD目前可能還有很多Bug，有一些功能還在完善中，之後還會陸續有更新完善功能和修正錯誤。

武漢加油！
願人類戰勝病毒！

更新日誌:
1.4
- 優化多邊形生成演算法
- 糾正了樓梯附近月臺天花板的多邊形生成錯誤
- 糾正了坡道月臺上的牆面傾斜的錯誤
- 糾正了一些UV映射
- 增加了在已經完成的車站上新增站廳和入口的功能（需要將過街地道升級為1.3版本）
- 重制了圖示
- 改進了內部連接道路的生成方式
1.3
- 修正了一可能的奔潰
1.2
- 修正了一些牆面閃爍的問題
- 修正了在未完成修建的月臺上連接軌道會出現隧道入口的問題
1.1
- 修復了和荷蘭車站模組的衝突
- 修復了非調試模式下的崩潰
- 修正了錯誤的開門方向
- 在建造參數菜單中增加了“月臺寬度”選項
- 在車站入口處增加了站名牌
- 增加了月臺號牌
- 改進了修改車站時的月臺編號穩定性
- 恢復了當只有一個月臺層時完成建造車站後修改車站參數的能力
- 實現了軌道升級功能
]]


function data()
    local profile = {
        en = {
            ["name"] = "Underground Station",
            ["desc"] = descEN,
            ["MENU_NAME"] = "Underground Station",
            ["MENU_DESC"] = "An underground level for a station, width adjustable platform width and radius.",
            ["MENU_FLOOR_STYLE"] = "Platform Floor Pattern",
            ["MENU_WALL"] = "Platform Wall Pattern",
            ["MENU_RADIUS"] = "Radius(m)",
            ["MENU_PLATFORM_LENGTH"] = "Platform Length(m)",
            ["MENU_PLATFORM_HEIGHT"] = "Platform Height(mm)",
            ["MENU_SLOPE"] = "Slope(‰)",
            ["MENU_TRACK_NR"] = "Track numbers",
            ["MENU_TRACK_CAT"] = "Electrified tracks",
            ["MENU_TRACK_HS"] = "High-Speed tracks",
            ["MENU_PLATFORM_LEFT"] = "Platform on the left",
            ["MENU_PLATFORM_RIGHT"] = "Platform on the right",
            ["MENU_FORCE_USE"] = "Force to use",
            ["MENU_PLATFORM_WIDTH"] = "Platform width(m)",
            ["MENU_PLATFORM_5_NAME"] = "5m wide platform",
            ["MENU_PLATFORM_5_DESC"] = "Underground platform of 5m wide",
            ["MENU_PLATFORM_10_NAME"] = "10m wide platform",
            ["MENU_PLATFORM_10_DESC"] = "Underground platform of 10m wide",
            ["MENU_PLATFORM_15_NAME"] = "15m wide platform",
            ["MENU_PLATFORM_15_DESC"] = "Underground platform of 15m wide",
            ["MENU_PLATFORM_20_NAME"] = "20m wide platform",
            ["MENU_PLATFORM_20_DESC"] = "Underground platform of 20m wide",
            ["MENU_TR_STD_NAME"] = "Standard track",
            ["MENU_TR_STD_DESC"] = "Standard track with speed limit of 120km/h",
            ["MENU_TR_STD_CAT_NAME"] = "Electrified track",
            ["MENU_TR_STD_CAT_DESC"] = "Electrified track with speed limit of 120km/h",
            ["MENU_TR_HS_NAME"] = "High-speed track",
            ["MENU_TR_HS_DESC"] = "Non-electrified high speed track with speed limit of 300km/h",
            ["MENU_TR_HS_CAT_NAME"] = "Electrified high-speed track",
            ["MENU_TR_HS_CAT_DESC"] = "High speed track with speed limit of 300km/h",
            ["MENU_UPSTAIRS_NAME"] = "Upstairs", 
            ["MENU_UPSTAIRS_DESC"] = "Stairs to a upper level or entry",
            ["MENU_DOWNSTAIRS_NAME"] = "Downstairs", 
            ["MENU_DOWNSTAIRS_DESC"] = "Stairs to a lower level",
            ["UNDERPASS_WARNING"] = 
[["Underground station" mod requires both "Underpass" mod and "Shader Enhancement" mod,
you will not be able to build up station and get correct rendering without either.]],
        },
        zh_CN = {
            ["name"] = "地下车站",
            ["desc"] = descCN,
            ["MENU_NAME"] = "地下车站",
            ["MENU_DESC"] = "可以修改站台宽度和半径的单层地下车站站厅",
            ["MENU_FLOOR_STYLE"] = "站台地面花纹",
            ["MENU_WALL"] = "侧墙花纹",
            ["MENU_RADIUS"] = "曲线半径(米)",
            ["MENU_PLATFORM_LENGTH"] = "站台长度(米)",
            ["MENU_PLATFORM_HEIGHT"] = "站台高度(毫米)",
            ["MENU_SLOPE"] = "站台坡度(‰)",
            ["MENU_TRACK_NR"] = "股道数",
            ["MENU_TRACK_CAT"] = "电气化股道",
            ["MENU_TRACK_HS"] = "高速铁路",
            ["MENU_PLATFORM_LEFT"] = "最左侧有站台",
            ["MENU_PLATFORM_RIGHT"] = "最右侧有站台",
            ["MENU_FORCE_USE"] = "直接使用",
            ["MENU_PLATFORM_WIDTH"] = "站台宽度(米)",
            ["MENU_PLATFORM_5_NAME"] = "5米宽站台",
            ["MENU_PLATFORM_5_DESC"] = "5米宽的地下车站站台",
            ["MENU_PLATFORM_10_NAME"] = "10米宽站台",
            ["MENU_PLATFORM_10_DESC"] = "10米宽的地下车站站台",
            ["MENU_PLATFORM_15_NAME"] = "15米宽站台",
            ["MENU_PLATFORM_15_DESC"] = "15米宽的地下车站站台",
            ["MENU_PLATFORM_20_NAME"] = "20米宽站台",
            ["MENU_PLATFORM_20_DESC"] = "20米宽的地下车站站台",
            ["MENU_TR_STD_NAME"] = "普速股道",
            ["MENU_TR_STD_DESC"] = "限速120km/h的非电气化车站股道",
            ["MENU_TR_STD_CAT_NAME"] = "电气化普速股道",
            ["MENU_TR_STD_CAT_DESC"] = "限速120km/h的电气化车站股道",
            ["MENU_TR_HS_NAME"] = "高速股道",
            ["MENU_TR_HS_DESC"] = "限速300km/h的非电气化车站股道",
            ["MENU_TR_HS_CAT_NAME"] = "电气化高速股道",
            ["MENU_TR_HS_CAT_DESC"] = "限速300km/h的电气化车站股道",
            ["MENU_UPSTAIRS_NAME"] = "升梯", 
            ["MENU_UPSTAIRS_DESC"] = "通往上层或车站入口的楼梯",
            ["MENU_DOWNSTAIRS_NAME"] = "降梯", 
            ["MENU_DOWNSTAIRS_DESC"] = "通往下层的楼梯",
            ["UNDERPASS_WARNING"] = [["地下车站模组需要“人行地道”和“着色器增强”模组方可使用，否则将无法正确渲染和建造车站。]],
        },
        zh_TW = {
            ["name"] = "地下車站",
            ["desc"] = descTC,           
            ["MENU_NAME"] = "地下車站",
            ["MENU_DESC"] = "可以修改月臺寬度和半徑的單層地下車站站廳",
            ["MENU_FLOOR_STYLE"] = "月臺地面花紋",
            ["MENU_WALL"] = "側牆花紋",
            ["MENU_RADIUS"] = "曲線半徑(米)",
            ["MENU_PLATFORM_LENGTH"] = "月臺長度(米)",
            ["MENU_PLATFORM_HEIGHT"] = "月臺高度(毫米)",
            ["MENU_SLOPE"] = "月臺坡度(‰)",
            ["MENU_TRACK_NR"] = "股道數",
            ["MENU_TRACK_CAT"] = "電氣化股道",
            ["MENU_TRACK_HS"] = "高速鐵路",
            ["MENU_PLATFORM_LEFT"] = "最左側有月臺",
            ["MENU_PLATFORM_RIGHT"] = "最右側有月臺",
            ["MENU_FORCE_USE"] = "直接使用",
            ["MENU_PLATFORM_WIDTH"] = "月臺寬度(米)",
            ["MENU_PLATFORM_5_NAME"] = "5米寬月臺",
            ["MENU_PLATFORM_5_DESC"] = "5米寬的地下車站月臺",
            ["MENU_PLATFORM_10_NAME"] = "10米寬月臺",
            ["MENU_PLATFORM_10_DESC"] = "10米寬的地下車站月臺",
            ["MENU_PLATFORM_15_NAME"] = "15米寬月臺",
            ["MENU_PLATFORM_15_DESC"] = "15米寬的地下車站月臺",
            ["MENU_PLATFORM_20_NAME"] = "20米寬月臺",
            ["MENU_PLATFORM_20_DESC"] = "20米寬的地下車站月臺",
            ["MENU_TR_STD_NAME"] = "普速股道",
            ["MENU_TR_STD_DESC"] = "限速120km/h的非電氣化車站股道",
            ["MENU_TR_STD_CAT_NAME"] = "電氣化普速股道",
            ["MENU_TR_STD_CAT_DESC"] = "限速120km/h的電氣化車站股道",
            ["MENU_TR_HS_NAME"] = "高速股道",
            ["MENU_TR_HS_DESC"] = "限速300km/h的非電氣化車站股道",
            ["MENU_TR_HS_CAT_NAME"] = "電氣化高速股道",
            ["MENU_TR_HS_CAT_DESC"] = "限速300km/h的電氣化車站股道",
            ["MENU_UPSTAIRS_NAME"] = "升梯", 
            ["MENU_UPSTAIRS_DESC"] = "通往上層或車站入口的樓梯",
            ["MENU_DOWNSTAIRS_NAME"] = "降梯", 
            ["MENU_DOWNSTAIRS_DESC"] = "通往下層的樓梯",
            ["UNDERPASS_WARNING"] = [["地下車站模組需要“人行地道”和“著色器增強”模組方可使用，否則將無法正確渲染和建造車站。]]
        }
    }
    return profile
end
