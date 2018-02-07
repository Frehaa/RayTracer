#include <iostream>
#include <fstream>
#include <cmath>

class Vector {
public:
    Vector() {
        x = 0.0;
        y = 0.0;
        z = 0.0;
    }
    Vector(double x, double y, double z) {
        this->x = x;
        this->y = y;
        this->z = z;
    }

    Vector operator-(const Vector& other) const {
        double x = this->x - other.x;
        double y = this->y - other.y;
        double z = this->z - other.z;
        return Vector(x, y, z);
    }

    Vector operator+(const Vector& other) const {
        double x = this->x + other.x;
        double y = this->y + other.y;
        double z = this->z + other.z;
        return Vector(x, y, z);
    }

    // Dot product
    double operator*(const Vector& other) const {
        return this->x * other.x + this->y * other.y + this->z * other.z;
    }

    Vector operator*(const double& constant) const {
        double x = this->x * constant;
        double y = this->y * constant;
        double z = this->z * constant;
        return Vector(x, y, z);
    }

    Vector operator/(const double& constant) const {
        double x = this->x / constant;
        double y = this->y / constant;
        double z = this->z / constant;
        return Vector(x, y, z);
    }

    Vector normalize() const {
        double mg = sqrt(this->x * this->x + this->y * this->y + this->z * this->z);
        return Vector(this->x / mg, this->y / mg, this->z / mg);
    }

private:
    double x;
    double y;
    double z;
};

class Ray {
public:
    Ray(Vector origin, Vector direction) {
        this->origin = origin;
        this->direction = direction;
    }

    const Vector& getOrigin() const {
        return origin;
    }

    const Vector& getDirection() const {
        return direction;
    }

private:
    Vector origin;
    Vector direction;
};

class Sphere {
public:
    Sphere(Vector center, double radius){
        this->center = center;
        this->radius = radius;
    }

    bool intersect(const Ray& ray, double &t) {
        Vector origin = ray.getOrigin();
        Vector direction = ray.getDirection();
        Vector diff = origin - this->center;

        double b = 2* (diff * direction);
        double c = diff * diff - this->radius * this->radius;
        double disc = b * b - 4 * c;
        if (disc < 1e-4) return false;
        else {
            disc = sqrt(disc);
            double t0 = -b-disc;
            double t1 = -b+disc;

            t = (t0 < t1) ? t0 : t1;
            return true;

        }

        return true;
    }

    Vector getNormal(Vector pi) {return (this->center-pi)/this->radius;}

    const Vector& getCenter() const {return this->center;}
    const double& getRadius() const {return this->radius;}

private:
    Vector center;
    double radius;
};

struct Color {
    double red;
    double green;
    double blue;

    Color() {
        this->red = 0;
        this->green = 0;
        this->blue = 0;
    }
    Color(double red, double green, double blue) {
        this->red = red;
        this->green = green;
        this->blue = blue;
    }
    Color operator*(double d) const {
        return Color(this->red * d, this->green * d, this->blue * d);
    }
};

std::ostream& operator<< (std::ostream& stream, const Color& pixel) {
    return stream << (int)pixel.red << ' ' << (int)pixel.green << ' ' << (int)pixel.blue << std::endl;;
};

int main(int argc, char** argv) {
    std::cout << "???" << std::endl;
    return 0;
    
    const int height = 500;
    const int width = 500;



    std::ofstream out("out.ppm");
    out << 'P' << '3' << '\n';
    out << width << ' ' << height << '\n';
    out << '2' << '5' << '5' << '\n';
        
    Color pixels[height][width];

    Color white(255, 255, 255);
    Sphere sphere(Vector(width/2, height/2, 50), 50);
    Sphere light(Vector(width/2, 0, 50), 1);

    // For each pixel
    for (int y = 0; y < height; ++y) {
        for (int x = 0; x < width; ++x) {
            // Send ray
            Ray ray(Vector(x, y, 0), Vector(0, 0, 1));

            double t = 20000;
            // 
            std::cout << x << ' ' << y << std::endl;
            if (sphere.intersect(ray, t)) {        
                std::cout << t << std::endl;        
                Vector pi = ray.getOrigin() + ray.getDirection() * t;

                Vector L = light.getCenter() - pi;
                Vector N = sphere.getNormal(pi);
                double dt = L.normalize() * N.normalize();

                std::cout << dt << std::endl;
                return 0;

                out << white * dt;

            } else {
                out << Color(0, 0, 0);;
            }
        }
    }

    return 0;
}