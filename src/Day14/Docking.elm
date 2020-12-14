module Day14.Docking exposing (puzzleInput, solve1, solve2)

import Basics.Extra exposing (flip)
import Binary exposing (Bits)
import Dict exposing (Dict)
import Parser as P exposing ((|.), (|=), Parser)


solve1 : String -> Int
solve1 =
    parseWith bitMaskParser >> runV1 >> .memory >> Dict.values >> List.sum


solve2 : String -> Int
solve2 =
    parseWith floatingMaskParser >> runV2 >> .memory >> Dict.values >> List.sum


type alias BitMask =
    { ones : Bits
    , zeroes : Bits
    }


type alias FloatingMask =
    List BitMask


type Instruction mask
    = SetMask mask
    | Write Int Int


type alias Program mask =
    { mask : mask
    , memory : Dict Int Int
    }


runV2 : List (Instruction FloatingMask) -> Program FloatingMask
runV2 =
    List.foldl
        (\instruction ({ memory, mask } as program) ->
            case instruction of
                SetMask newMask ->
                    { program | mask = newMask }

                Write loc value ->
                    { program
                        | memory =
                            List.map (flip applyMask loc) mask
                                |> List.foldl (flip Dict.insert value) memory
                    }
        )
        { mask = []
        , memory = Dict.empty
        }


runV1 : List (Instruction BitMask) -> Program BitMask
runV1 =
    List.foldl
        (\instruction ({ memory, mask } as program) ->
            case instruction of
                SetMask newMask ->
                    { program | mask = newMask }

                Write loc value ->
                    { program | memory = Dict.insert loc (applyMask mask value) memory }
        )
        { mask = initMask
        , memory = Dict.empty
        }


applyMask : BitMask -> Int -> Int
applyMask { ones, zeroes } =
    Binary.fromDecimal >> Binary.or ones >> Binary.and zeroes >> Binary.toDecimal


parseWith : Parser mask -> String -> List (Instruction mask)
parseWith maskParser =
    String.lines >> List.filterMap (P.run (instructionParser maskParser) >> Result.toMaybe)


instructionParser : Parser mask -> Parser (Instruction mask)
instructionParser mask =
    P.oneOf
        [ P.succeed SetMask
            |. P.keyword "mask"
            |. P.spaces
            |. P.symbol "="
            |. P.spaces
            |= mask
        , P.succeed Write
            |. P.keyword "mem"
            |. P.symbol "["
            |= P.int
            |. P.symbol "]"
            |. P.spaces
            |. P.symbol "="
            |. P.spaces
            |= P.int
            |. P.end
        ]


bitMaskParser : Parser BitMask
bitMaskParser =
    P.loop ( binaryWidth, initMask ) <|
        \( place, mask ) ->
            let
                loop =
                    Tuple.pair (place - 1) >> P.Loop
            in
            P.oneOf
                [ P.succeed (loop mask)
                    |. P.token "X"
                , P.succeed (loop (mask |> withOneAt place))
                    |. P.token "1"
                , P.succeed (loop (mask |> withZeroAt place))
                    |. P.token "0"
                , P.succeed (P.Done mask)
                    |. P.end
                ]


floatingMaskParser : Parser FloatingMask
floatingMaskParser =
    P.loop ( binaryWidth, [ initMask ] ) <|
        \( place, mask ) ->
            let
                loop =
                    Tuple.pair (place - 1) >> P.Loop
            in
            P.oneOf
                [ P.succeed (loop mask)
                    |. P.token "0"
                , P.succeed (loop <| List.map (withOneAt place) mask)
                    |. P.token "1"
                , P.succeed (loop (mask |> branchAt place))
                    |. P.token "X"
                , P.succeed (P.Done mask)
                    |. P.end
                ]


withOneAt : Int -> BitMask -> BitMask
withOneAt place mask =
    { mask | ones = Binary.or mask.ones <| oneAt place }


withZeroAt : Int -> BitMask -> BitMask
withZeroAt place mask =
    { mask | zeroes = Binary.and mask.zeroes <| onesWithZeroAt place }


branchAt : Int -> FloatingMask -> FloatingMask
branchAt place mask =
    List.map (withOneAt place) mask ++ List.map (withZeroAt place) mask


oneAt : Int -> Bits
oneAt place =
    True
        :: List.repeat place False
        |> Binary.fromBooleans


onesWithZeroAt : Int -> Bits
onesWithZeroAt place =
    List.repeat (binaryWidth - place) True
        ++ (False :: List.repeat place True)
        |> Binary.fromBooleans


initMask : BitMask
initMask =
    { ones = Binary.empty
    , zeroes = Binary.fromIntegers <| List.repeat 36 1
    }


binaryWidth : Int
binaryWidth =
    35


puzzleInput =
    """mask = 100X100X101011111X100000100X11010011
mem[33323] = 349380
mem[52742] = 116688965
mem[4113] = 11499
mem[15819] = 313303
mem[23239] = 755579063
mask = X00X10X1X010110110111X00010X100X000X
mem[49207] = 466621685
mem[34069] = 6874604
mask = 1001100XX00011110110100XX0110000001X
mem[61278] = 56361674
mem[51360] = 61871432
mem[31903] = 45067
mask = 100X100XX0101X11X1X00X00001001X101X0
mem[22981] = 144008
mem[12013] = 49165315
mem[54643] = 50677
mem[59166] = 678129
mem[64022] = 27522
mask = 100110X0001X11011000101X1000001X00X0
mem[32693] = 425145
mem[11437] = 236593490
mem[16078] = 227582
mem[35266] = 197465438
mem[39279] = 127942
mask = 10101000X0X00001X1001000010100111X00
mem[49794] = 2082
mem[60407] = 2129
mem[33300] = 921
mem[18582] = 62106545
mem[32160] = 843912
mem[36917] = 7740
mem[1836] = 54721591
mask = 100010X1X0X011X1101XX00001X01000X10X
mem[8385] = 1381
mem[38022] = 2501412
mem[34713] = 3648024
mem[33245] = 1178087
mem[22176] = 263
mem[20535] = 1289
mem[2092] = 88590569
mask = X001100X00X0X01X0X100X100010110XX101
mem[65061] = 2768
mem[56375] = 6734
mem[18070] = 20571066
mem[61511] = 403157281
mem[4164] = 179682
mem[11801] = 5501
mem[22339] = 14414879
mask = X0011000001X1001X0100111110X00110111
mem[3844] = 1046
mem[33741] = 109390
mem[54311] = 94183595
mem[48744] = 112575
mem[29663] = 2042
mask = X00X100000101001101001001X00001000X1
mem[25325] = 177269
mem[919] = 50779835
mem[52113] = 2386630
mem[60154] = 29645195
mem[24761] = 8101
mask = X101X000X01011011010X100001101110X01
mem[5169] = 2865
mem[55126] = 50829
mem[60154] = 124556261
mem[48753] = 377574
mem[48662] = 9144531
mask = 10011X00001010011010000101110XX0X00X
mem[41623] = 632353121
mem[10365] = 70888870
mem[59458] = 849
mem[18992] = 486294339
mask = X00X100X011011111100X00001001010100X
mem[42046] = 518245944
mem[4654] = 39071
mem[46109] = 1540
mem[3245] = 822
mem[25937] = 257692
mem[19118] = 6601278
mask = 1001001XXX100XXX101XX0001010001000X0
mem[34356] = 55967
mem[52601] = 522574
mem[31903] = 7669828
mem[36165] = 10552
mask = 110X101X00X0111111XX001X0001000XX10X
mem[42649] = 1534730
mem[8324] = 467628
mem[9447] = 3054
mem[41788] = 28205
mem[9353] = 14315559
mask = 1X01X01100111111X101000000X100100000
mem[270] = 3208
mem[20373] = 186089492
mem[43940] = 449607191
mem[63389] = 674
mem[437] = 6933780
mask = 1001X00001X01X0X101101X0010X00110110
mem[22829] = 3301
mem[59260] = 6763
mem[22305] = 203360
mask = 10011110101010X010XX011X0010001XX000
mem[55041] = 6199
mem[55452] = 151
mem[2746] = 464657
mask = 1001000000X0X10110X01101X00100111000
mem[54354] = 666913
mem[44827] = 214920
mem[44621] = 13259544
mem[29462] = 14725
mem[27633] = 284739975
mem[63195] = 11668372
mask = 10X010X10100X111001X101010101X11100X
mem[21667] = 426958
mem[55530] = 91533
mem[10365] = 493
mem[51246] = 513589450
mem[44622] = 1773
mem[4113] = 401
mask = 100X1000001011XXX0100XXX100010X10X00
mem[60407] = 869913
mem[10365] = 59083
mem[18321] = 3019
mem[65061] = 10794134
mem[62827] = 2777572
mem[20373] = 23798334
mask = 1000X10011X010011X10X0000101X0100001
mem[17936] = 4347
mem[38270] = 611
mem[7408] = 2854792
mem[2612] = 604172
mem[24287] = 418220
mem[27110] = 31440
mem[64742] = 1872667
mask = 10X110000010100110X001X01X1000000111
mem[30518] = 13431
mem[64496] = 204238
mem[62259] = 1191
mem[17457] = 3652
mask = 100X1X0XX1101XX11010X000X01010010011
mem[25325] = 67829
mem[4021] = 8039
mask = 1XXXXX0X0010110X11100111001111101110
mem[34600] = 4128134
mem[47565] = 28022073
mask = X0X110000XX010X10010X0X111X111010101
mem[64746] = 17532220
mem[55786] = 109034
mem[12715] = 185475
mask = 1001110X011010111010X1010010100XX100
mem[28923] = 1444
mem[7508] = 41968
mem[39856] = 447
mem[19698] = 4420683
mem[60924] = 7222
mem[8056] = 225410214
mask = 100X10X1X0X011X10110X01X011000X10X00
mem[58206] = 585282
mem[10984] = 105158307
mem[31562] = 526874
mem[60154] = 107013
mem[4409] = 4126230
mask = 1010100010X0XX0111X00X00011X000X0XX0
mem[7122] = 428629
mem[29394] = 262029322
mem[33832] = 6067254
mask = 0001100XXX0010X001100010X000110001X1
mem[1975] = 32392
mem[14891] = 9350
mem[19905] = 28213400
mem[11981] = 132973999
mem[49582] = 4347
mem[64106] = 235564
mem[9648] = 1440
mask = 000110010011XXXX0X1001010001X00X0100
mem[18992] = 628
mem[37263] = 1031
mem[4387] = 1442306
mem[2471] = 1123350
mem[1493] = 88891215
mem[22500] = 3553
mem[6845] = 26007
mask = 10011X00011011X1101X00X001X1X001X111
mem[49101] = 13289
mem[32] = 391365
mem[31906] = 79
mem[48744] = 71043
mask = 1001X0X00010100110X001011001101X01X0
mem[25999] = 2473051
mem[36408] = 56819077
mem[46656] = 2074748
mem[10871] = 8606
mem[7122] = 2053
mem[59403] = 5442
mask = 1XX0X01X100X11111010X000X00X000101X0
mem[1160] = 280063168
mem[20571] = 19030
mem[23225] = 51089295
mem[40992] = 17475
mem[63413] = 1144
mem[19458] = 284777610
mem[21502] = 10410
mask = 100X100X00101X0100X0X0X11100111XX11X
mem[33860] = 160
mem[37007] = 56420
mem[55140] = 490726
mem[47752] = 521745
mem[55594] = 336661995
mem[44008] = 265991679
mask = 1001100001X010011100100X01X0X011111X
mem[1289] = 55191
mem[53058] = 23079796
mem[25362] = 57315626
mem[8895] = 35287816
mask = 0001100100XX00100X1X0X00XX00XX000110
mem[12568] = 136661
mem[9931] = 303487
mem[38781] = 91532
mem[25506] = 950257996
mem[3694] = 6225663
mem[6631] = 62710499
mem[3205] = 7586715
mask = X0001000001X111110100000X0110001000X
mem[61696] = 34763
mem[42583] = 2987088
mem[8416] = 2293694
mem[21503] = 8071
mem[41788] = 950960
mem[9648] = 23284946
mask = 100010000010XX0XX01000000X011100X1X0
mem[30270] = 421
mem[52379] = 86815089
mem[16627] = 3647190
mem[36794] = 132421727
mem[54580] = 248096
mask = 10X1101000X01001X00111110101110001X0
mem[48399] = 9196559
mem[6869] = 32793911
mem[20422] = 1560
mem[12101] = 15618
mem[25154] = 390003034
mem[23791] = 229770864
mem[49558] = 12206144
mask = 100X10010X101XX1XXX000001X00101X1100
mem[3205] = 110968351
mem[65515] = 7362194
mem[2197] = 52580964
mem[13004] = 3723834
mem[46931] = 24935229
mem[919] = 6284
mask = 10001X11100X1X1X10X111X100X0000010X1
mem[30162] = 1665
mem[35687] = 3554
mem[3735] = 8003
mem[18258] = 44276232
mem[48625] = 401841687
mem[62781] = 2814958
mem[5302] = 175144514
mask = 1001X0XX001X101110101000X11X00010X00
mem[38152] = 42369373
mem[36392] = 13302
mem[13867] = 940605082
mask = 10001100X11010X1101000X0X11100110011
mem[63412] = 5289
mem[788] = 6600
mem[27915] = 254034
mem[24347] = 16264001
mem[52437] = 651358
mask = 10011X0X0110X0X11X101100101100X11100
mem[56524] = 1244173
mem[64911] = 2124386
mem[3815] = 107466
mem[14375] = 6798
mem[16285] = 66968238
mem[7968] = 835823180
mask = 10X110100X101XX110X11110XX0111001010
mem[58730] = 132998954
mem[8056] = 754181
mem[39247] = 126
mask = 1001X000001XX10110101X1110110X10101X
mem[59028] = 10817
mem[17977] = 61299509
mask = 1X001100X1X0100110100000X111XXX001X0
mem[2056] = 32701076
mem[2071] = 2401082
mem[9887] = 998417
mask = 100110X11X101X1110X00100X0101111X0X1
mem[33860] = 388064
mem[59050] = 16623098
mem[5188] = 319
mem[37207] = 2470432
mem[27333] = 2026
mask = 1000X000001X1X0X00XXX00X100011X11010
mem[24029] = 9105
mem[14364] = 243545984
mem[4113] = 3279
mask = 1X0X1001X0101011110XX1000100X000X101
mem[17781] = 509963835
mem[37716] = 62611707
mem[23997] = 1023138975
mem[5927] = 32777
mem[55304] = 264062857
mask = 100110X001X01X01100011100X100110X11X
mem[58338] = 741
mem[34693] = 991498
mem[32339] = 30979944
mem[50216] = 66393532
mem[29090] = 11574321
mem[30824] = 15729
mem[16868] = 23942
mask = 1X0XX0010X0011110X101010111011111010
mem[48969] = 3327849
mem[52521] = 460105388
mem[33860] = 422661865
mem[44621] = 6715
mem[27762] = 11952
mem[34536] = 4064
mask = 1001X001001X0011001000100110010001XX
mem[195] = 487302
mem[17992] = 889
mem[11858] = 958195
mem[11013] = 202443463
mask = 1000101X100X1111011000X100110000X001
mem[13097] = 3534
mem[41292] = 85120
mem[9497] = 154119
mem[19610] = 5709354
mem[34972] = 48311
mem[50753] = 180578
mem[35921] = 667946365
mask = XX1010X00110X00111000XX00001000110X0
mem[3712] = 2843518
mem[34604] = 2965
mem[54311] = 162583
mask = 0001X0X00100100X001000001X1X10X01X10
mem[49406] = 965493
mem[59050] = 392048
mem[3574] = 922708604
mem[7419] = 33525859
mem[1933] = 8
mem[4367] = 11521
mask = 1001X0X00X10X00X101X00001110X0100X00
mem[29215] = 417522
mem[56468] = 34229032
mem[26868] = 552971
mem[36368] = 420213
mask = 100110X0X1101011101X01X01101101X001X
mem[4913] = 455
mem[3815] = 11211510
mem[21545] = 1469
mem[35762] = 1806
mem[58825] = 3743
mem[23225] = 474872535
mem[53173] = 46538
mask = 1XX0X00X0X101001001010100X0X01X00010
mem[64106] = 98247289
mem[13686] = 54961348
mem[38944] = 462290318
mem[53185] = 7075
mem[30162] = 39454
mem[14983] = 1010603
mem[38339] = 970
mask = X001100X010X111110001000001X01100110
mem[12827] = 22328
mem[18628] = 7082210
mem[31013] = 20804915
mem[13966] = 86
mem[518] = 1757
mask = X001100XX001001001110000000000XX1110
mem[14375] = 8414661
mem[1568] = 225486
mem[25775] = 336197
mask = 100110000X00100X100001100X111X100X01
mem[2071] = 51386682
mem[32897] = 162194
mem[11308] = 1799417
mem[20829] = 299249
mask = 1X0010XXX0001111XX1100X001X1X0000101
mem[29189] = 36530
mem[657] = 114543286
mem[9356] = 451
mask = X000100000101X0110X0011XX10000110001
mem[30577] = 117881
mem[60874] = 19567558
mem[10363] = 13493
mem[5690] = 382
mem[61059] = 4757304
mem[36165] = 95983791
mask = 100X00X00010100X1010000X101000X10000
mem[33324] = 39476477
mem[34713] = 7398
mem[46214] = 98709
mem[35856] = 1020446010
mask = 10X01X000010000X11100101011X001X0100
mem[65061] = 61054
mem[54052] = 92826
mem[35603] = 58759
mem[58037] = 40910
mem[62217] = 45701380
mask = 1X011000001011011010XX00X10X0X010001
mem[15920] = 5645
mem[28828] = 265910022
mem[29437] = 5544
mem[56112] = 637
mem[45033] = 36063036
mem[12783] = 13776458
mask = 10011011X010100110XX100100XX11011X00
mem[518] = 25998191
mem[13053] = 7866406
mem[38152] = 3208
mem[18730] = 711
mask = 10X11000001XX1X000100X11101XX1X10111
mem[47121] = 11272115
mem[43618] = 27683
mask = 100X1101X0101001100X010000X11001X100
mem[21702] = 34688805
mem[43624] = 3956780
mem[24476] = 17239393
mem[23321] = 25573609
mem[15163] = 1713
mem[65338] = 27386792
mask = 10011010010X10011X0011110XX100001111
mem[53501] = 16700270
mem[28069] = 20683243
mem[33593] = 114830
mem[9962] = 403282549
mem[54061] = 2336
mem[46656] = 7039
mem[58616] = 181
mask = 10001X11001011XX101X0100010010101100
mem[8738] = 234383093
mem[11512] = 1792627
mem[54326] = 1574223
mask = 10011X101X10100XX000X10010X01X01100X
mem[51382] = 17879
mem[44905] = 783
mem[57514] = 1018128542
mem[18628] = 240492
mem[2108] = 3429
mem[2304] = 3748
mask = 0X011001X0X000X000110100101000000000
mem[4452] = 19437119
mem[64742] = 179090
mem[16430] = 486207
mask = 1001X000111X1X1110110X011111100X0011
mem[52004] = 41486
mem[48779] = 83675
mem[17861] = 48577395
mem[39247] = 16952
mem[8738] = 3981
mem[32923] = 1168904
mask = 10011001XX0011X11X0010X1010X10101000
mem[33319] = 44401
mem[4142] = 517003945
mem[29189] = 415157
mem[33358] = 1395165
mask = 1001100X010010011XX00XXX1100101X0101
mem[13618] = 246280673
mem[58338] = 17884
mem[10885] = 816
mem[11277] = 24331199
mem[17936] = 1616051
mask = 1001100X01X01001X00011000110X0X001XX
mem[58338] = 302363844
mem[53596] = 175604903
mem[56468] = 419729
mem[27915] = 581
mem[41501] = 69718
mask = 100110000X1010110100X1X001X00X01001X
mem[18333] = 15544
mem[3929] = 2622169
mem[37718] = 176413
mem[27333] = 848
mem[17456] = 1097
mask = 100110101010X001X000X0001X00001011X0
mem[53045] = 2356198
mem[49908] = 1086
mem[17019] = 7107107
mem[12013] = 70971
mem[7048] = 1585
mem[3666] = 4937143
mask = 10011XXXX01010011000010X1X11100XXX00
mem[65524] = 4129175
mem[5636] = 315661
mem[39270] = 455882795
mask = X1001100110010011010000001110X0X00XX
mem[50481] = 26734
mem[57708] = 199726127
mem[20422] = 130991
mem[13651] = 1094687
mem[1292] = 60536
mask = 110X1011001XX111110100X0000010X1X101
mem[39644] = 14574
mem[8596] = 30400
mask = 1000101XX0X0X1110110111X110011X00110
mem[919] = 32148
mem[41] = 453324
mem[36794] = 179133
mem[2780] = 958033590
mask = 100010X1X11011110010X01101101110X100
mem[20035] = 1674335
mem[18909] = 33271
mem[21491] = 4013451
mem[21792] = 78760
mem[42156] = 980
mem[3276] = 3971405
mask = 10XX10000X10X00111X0XX00X0010011X100
mem[36368] = 5097527
mem[3099] = 104365
mem[57092] = 74461253
mem[46314] = 30483860
mask = X000101X101011X10110XXX001X00X11X010
mem[9948] = 43011947
mem[53185] = 41588
mem[25699] = 101124
mem[60046] = 123243
mem[23975] = 125991
mask = 1X00100000X01X00101011100X1010000101
mem[65101] = 504575
mem[55313] = 14953613
mem[42156] = 526
mem[55573] = 1303957
mem[53260] = 16252
mem[48073] = 8667
mask = 1001100100101X11110X0X0X0111X00001X0
mem[10402] = 793546
mem[45910] = 18
mem[23627] = 72728
mem[7408] = 16579752
mem[22105] = 10576
mem[61054] = 1160961
mem[2989] = 149675383
mask = 0001X001000000XX0111X110010001010110
mem[15867] = 14
mem[23379] = 10511918
mem[4217] = 4840435
mem[29978] = 11828937
mem[28303] = 2358671
mask = 10010010011X0XX11010X000110000110X00
mem[11923] = 149358903
mem[46246] = 3148
mem[17596] = 9370
mem[1540] = 12848
mem[25775] = 29444
mem[32564] = 64008
mem[16097] = 641
mask = 0X011001X010X010X0100X1X0X0X000111X1
mem[45770] = 1008133
mem[15551] = 3912928
mem[53058] = 188856
mem[44827] = 9036496
mem[59530] = 20033543
mask = 1001100X0XX01X0110X000XX010010XX1101
mem[2056] = 737
mem[34972] = 30655
mem[50728] = 927954
mask = 10001X0000X0X0010010101010X001100110
mem[39247] = 425181
mem[64200] = 13111811
mem[8169] = 1250162
mask = 100110000X10XXX11010X0001110X011XX00
mem[62259] = 4350710
mem[56112] = 42327
mem[53173] = 2221557
mem[36759] = 242686307
mem[29077] = 1179326
mem[2056] = 356
mask = 10000000001XX000101000X0X11000X10110
mem[18542] = 454113
mem[44192] = 501708
mem[54994] = 149470837
mem[54260] = 582959
mem[65424] = 295679271
mem[36368] = 2002
mem[16392] = 99
mask = 10100001XX101001X0101100101101000XX0
mem[17861] = 3340321
mem[24705] = 4143350
mem[38940] = 201585
mem[35632] = 19204465
mem[9443] = 5273035
mask = 10X110010010100101000X00001010X0111X
mem[2991] = 51624
mem[56468] = 1603
mem[35633] = 4068
mask = 10011X01001010X10000X011000111101X11
mem[58842] = 69158
mem[43765] = 1624
mem[24913] = 133864698
mem[15015] = 247
mem[10155] = 1064
mem[33787] = 142284522
mem[17457] = 15488682"""
