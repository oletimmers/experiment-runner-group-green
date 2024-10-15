#include <iostream>
#include <vector>

int main() {
    std::vector<std::string> names = {"Debdutta", "Padma", "Summer", "Kaustav", "Ole"};
    
    for (const auto& name : names) {
        std::cout << "Hello " << name << "!" << std::endl;
    }

    return 0;
}