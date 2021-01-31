#include <iostream>
#include <cstdint>

extern "C" std::int32_t f(std::int32_t i) {
    std::cout << "hello " << i << std::endl;
    return 3;
}