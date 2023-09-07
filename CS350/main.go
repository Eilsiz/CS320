package main

import ("fmt"
     
)


func main() {
    fmt.Println("Hello World!")
    var input string 
    var err error
    _, err = fmt.Scanln(&input)

    if err!= nil{
        fmt.Println("Error -", err)
     } else{
        fmt.Println("You are successed entered - ", input)
     }
}