#ifndef SAMPLE_HPP
#define SAMPLE_HPP

#include <string>
#include <vector>
#include <memory>
#include "types.h"

namespace geometry {

class Shape {
public:
    virtual double area() const = 0;
    virtual ~Shape() = default;
};

struct Point {
    double x;
    double y;
};

template <typename T>
class Container {
public:
    void add(const T& item);
    T get(int index) const;
private:
    std::vector<T> items_;
};

enum class Color {
    Red,
    Green,
    Blue
};

} // namespace geometry

#endif // SAMPLE_HPP
