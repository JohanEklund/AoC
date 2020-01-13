#load "./AoCUtil.fsx"

open AoCUtil

let FuelCalc mass =
    (mass / 3) - 2

let rec TotalFuelCalc mass = 
    let fuel = FuelCalc mass
    if fuel < 1 then 0
    else fuel + TotalFuelCalc fuel

let UpgA =
    let inputFile = "./AoC2019/Day1/inputA.txt"
    let input = readIntLines inputFile
    List.sumBy FuelCalc input

let UpgB =
    let inputFile = "./AoC2019/Day1/inputA.txt"
    let input = readIntLines inputFile
    List.sumBy TotalFuelCalc input