module Day01.Report exposing (findPairsSummingTo, puzzleInput, solve1, solve2)

import List.Extra
import Set exposing (Set)


solve1 : String -> Maybe ( Int, Int )
solve1 =
    parseInput >> findPairsSummingTo 2020


solve2 : String -> Maybe ( Int, Int, Int )
solve2 =
    parseInput >> findThreesSummingTo 2020


findPairsSummingTo : Int -> List Int -> Maybe ( Int, Int )
findPairsSummingTo sum =
    let
        go : Set Int -> List Int -> Maybe ( Int, Int )
        go seen input =
            List.Extra.uncons input
                |> Maybe.andThen
                    (\( x, xs ) ->
                        let
                            complement =
                                sum - x
                        in
                        if Set.member complement seen then
                            Just ( x, complement )

                        else
                            go (Set.insert x seen) xs
                    )
    in
    go Set.empty


findThreesSummingTo : Int -> List Int -> Maybe ( Int, Int, Int )
findThreesSummingTo sum =
    let
        go : List Int -> List Int -> Maybe ( Int, Int, Int )
        go seen input =
            case input of
                [] ->
                    Nothing

                x :: xs ->
                    case findPairsSummingTo (sum - x) (seen ++ xs) of
                        Nothing ->
                            go (x :: seen) xs

                        Just ( a, b ) ->
                            Just ( a, b, x )
    in
    go []


parseInput : String -> List Int
parseInput =
    String.lines >> List.filterMap String.toInt


puzzleInput : String
puzzleInput =
    """1645
1995
1658
1062
1472
1710
1424
1823
1518
1656
1811
1511
1320
1521
1395
1996
1724
1666
1637
1504
1766
534
1738
1791
1372
1225
1690
1949
1495
1436
1166
1686
1861
1889
1887
997
1202
1478
833
1497
1459
1717
1272
1047
1751
1549
1204
1230
1260
1611
1506
1648
1354
1415
1615
1327
1622
1592
1807
1601
1026
1757
1376
1707
1514
1905
1660
1578
1963
1292
390
1898
1019
1580
1499
1830
1801
1881
1764
1442
1838
1088
1087
1040
1349
1644
1908
1697
1115
1178
1224
1810
1445
1594
1894
1287
1676
1435
1294
1796
1350
1685
1118
1488
1726
1696
1190
1538
1780
1806
1207
1346
1705
983
1249
1455
2002
1466
1723
1227
1390
1281
1715
1603
1862
1744
1774
1385
1312
1654
1872
1142
1273
1508
1639
1827
1461
1795
1533
1304
1417
1984
28
1693
1951
1391
1931
1179
1278
1400
1361
1369
1343
1416
1426
314
1510
1933
1239
1218
1918
1797
1255
1399
1229
723
1992
1595
1191
1916
1525
1605
1524
1869
1652
1874
1756
1246
1310
1219
1482
1429
1244
1554
1575
1123
1194
1408
1917
1613
1773
1809
1987
1733
1844
1423
1718
1714
1923
1503"""
