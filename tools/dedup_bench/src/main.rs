fn main() {
    println!("type unit;");
    println!("type bool;");
    println!("type int;");
    println!("type char;");
    println!("type float;");
    println!("type string;");

    for i in 0..99 {
        for _ in 0..100 {
            println!("{{");

            println!("where a{0}i = {0} + {0};", i);
            println!("where a{0}f = {0}.{0} + {0}.{0};", i);

            println!("}};");
        }
    }
}
