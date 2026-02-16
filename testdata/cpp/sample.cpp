#include <iostream>
#include <string>
#include <vector>
#include <algorithm>
#include "sample.hpp"
#include "utils.hpp"

using namespace std;
using std::shared_ptr;
using std::make_shared;

#define PI 3.14159
#define SQUARE(x) ((x) * (x))

/* Multi-line comment
 * class FakeClass {} should be ignored
 */

namespace geometry {

class Circle : public Shape {
public:
    Circle(double radius) : radius_(radius) {}

    double area() const override {
        return PI * radius_ * radius_;
    }

    double radius() const { return radius_; }

private:
    double radius_;
};

struct Rectangle {
    double width;
    double height;
    double area() const { return width * height; }
};

template <typename T>
T max_value(T a, T b) {
    return (a > b) ? a : b;
}

template <typename T, typename U>
class Pair {
public:
    Pair(T first, U second) : first_(first), second_(second) {}
    T first() const { return first_; }
    U second() const { return second_; }
private:
    T first_;
    U second_;
};

enum class Priority {
    Low,
    Medium,
    High
};

enum OldStyle {
    VALUE_A,
    VALUE_B,
    VALUE_C
};

} // namespace geometry

namespace utils {

void log_message(const string& msg) {
    cout << msg << endl;
}

static void internal_helper() {
    // not exported
}

} // namespace utils

int main(int argc, char** argv) {
    geometry::Circle c(5.0);
    cout << c.area() << endl;
    return 0;
}
