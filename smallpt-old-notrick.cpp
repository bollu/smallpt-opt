#include <math.h>    // smallpt, a Path Tracer by Kevin Beason, 2008
#include <stdio.h>   //        Remove "-fopenmp" for g++ version < 4.2
#include <stdlib.h>  // Make : g++ -O3 -fopenmp smallpt.cpp -o smallpt
 
extern "C" { const char *__asan_default_options() { return "detect_leaks=0"; } };

struct Vec {         // Usage: time ./smallpt 5000 && xv image.ppm
    double x, y, z;  // position, also color (r,g,b)
    Vec(double x_, double y_, double z_) {
        x = x_;
        y = y_;
        z = z_;
    }
    Vec() { x = y = z = 0; }

    static Vec zero() { return Vec(0, 0, 0); }
    Vec operator+(const Vec &b) const { return Vec(x + b.x, y + b.y, z + b.z); }
    Vec operator-(const Vec &b) const { return Vec(x - b.x, y - b.y, z - b.z); }
    Vec operator*(double b) const { return Vec(x * b, y * b, z * b); }
    Vec mult(const Vec &b) const { return Vec(x * b.x, y * b.y, z * b.z); }
    // Vec& norm(){ return *this = *this * (1/sqrt(x*x+y*y+z*z)); }
    Vec norm() const { return *this * (1 / sqrt(x * x + y * y + z * z)); }
    double dot(const Vec &b) const {
        return x * b.x + y * b.y + z * b.z;
    }  // cross:
    Vec operator%(const Vec &b) const {
        return Vec(y * b.z - z * b.y, z * b.x - x * b.z, x * b.y - y * b.x);
    }
};
struct Ray {
    Vec o, d;
    Ray(Vec o_, Vec d_) : o(o_), d(d_) {}
};
enum Refl_t { DIFF, SPEC, REFR };  // material types, used in radiance()
struct Sphere {
    double rad;   // radius
    Vec p, e, c;  // position, emission, color
    Refl_t refl;  // reflection type (DIFFuse, SPECular, REFRactive)
    Sphere(double rad_, Vec p_, Vec e_, Vec c_, Refl_t refl_)
        : rad(rad_), p(p_), e(e_), c(c_), refl(refl_) {}
    double intersect(const Ray &r) const {  // returns distance, 0 if nohit
        Vec op = p - r.o;  // Solve t^2*d.d + 2*t*(o-p).d + (o-p).(o-p)-R^2 = 0
        double t, eps = 1e-4, b = op.dot(r.d),
                  det = b * b - op.dot(op) + rad * rad;
        if (det < 0) {
            return 0;
        } else {
            det = sqrt(det);
        }
        return (t = b - det) > eps ? t : ((t = b + det) > eps ? t : 0);
    }
};
Sphere spheres[] = {
    // Scene: radius, position, emission, color, material
    Sphere(1e5, Vec(1e5 + 1, 40.8, 81.6), Vec::zero(), Vec(.75, .25, .25),
           DIFF),  // Left
    Sphere(1e5, Vec(-1e5 + 99, 40.8, 81.6), Vec::zero(), Vec(.25, .25, .75),
           DIFF),  // Rght
    Sphere(1e5, Vec(50, 40.8, 1e5), Vec::zero(), Vec(.75, .75, .75),
           DIFF),  // Back
    Sphere(1e5, Vec(50, 40.8, -1e5 + 170), Vec::zero(), Vec::zero(),
           DIFF),  // Frnt
    Sphere(1e5, Vec(50, 1e5, 81.6), Vec::zero(), Vec(.75, .75, .75),
           DIFF),  // Botm
    Sphere(1e5, Vec(50, -1e5 + 81.6, 81.6), Vec::zero(), Vec(.75, .75, .75),
           DIFF),  // Top
    Sphere(16.5, Vec(27, 16.5, 47), Vec::zero(), Vec(1, 1, 1) * .999,
           SPEC),  // Mirr
    Sphere(16.5, Vec(73, 16.5, 78), Vec::zero(), Vec(1, 1, 1) * .999,
           REFR),  // Glas
    Sphere(600, Vec(50, 681.6 - .27, 81.6), Vec(12, 12, 12), Vec::zero(),
           DIFF)  // Lite
};
inline double clamp(double x) { return x < 0 ? 0 : x > 1 ? 1 : x; }
inline int toInt(double x) { return int(pow(clamp(x), 1 / 2.2) * 255 + .5); }

inline bool intersect(const Ray &r, double &t, int &id) {
    const double n = sizeof(spheres) / sizeof(Sphere), inf = t = 1e20;
    for (int i = int(n); i--;) {
        const double d = spheres[i].intersect(r);
        if (d && d < t) {
            t = d;
            id = i;
        }
    }
    return t < inf;
}
Vec radiance(const Ray &r, int depth, unsigned short *Xi) {
    double t;    // distance to intersection
    int id = 0;  // id of intersected object
    if (!intersect(r, t, id)) {
        return Vec::zero();
    }                                 // if miss, return black
    const Sphere &obj = spheres[id];  // the hit object
    const Vec x = r.o + r.d * t, n = (x - obj.p).norm(),
              nl = n.dot(r.d) < 0 ? n : n * -1;
    Vec f = obj.c;  // MUTATION
    const double p = fmax(fmax(f.x, f.y), f.z);
    const double depthnew = depth + 1;
    if (depthnew > 5) {
        if (erand48(Xi) < p) {
            f = f * (1 / p);  // MUTATION
        } else {
            return obj.e;  // R.R.
        }
    }

    if (obj.refl == DIFF) {  // Ideal DIFFUSE reflection
        const double r1 = 2 * M_PI * erand48(Xi), r2 = erand48(Xi),
                     r2s = sqrt(r2);
        const Vec w = nl,
                  u = ((fabs(w.x) > .1 ? Vec(0, 1, 0) : Vec(1, 0, 0)) % w)
                          .norm(),
                  v = w % u;
        const Vec d =
            (u * cos(r1) * r2s + v * sin(r1) * r2s + w * sqrt(1 - r2)).norm();
        return obj.e + f.mult(radiance(Ray(x, d), depthnew, Xi));
    } else if (obj.refl == SPEC) {  // Ideal SPECULAR reflection
        return obj.e +
               f.mult(radiance(Ray(x, r.d - n * 2 * n.dot(r.d)), depthnew, Xi));
    }
    const Ray reflRay(x, r.d - n * 2 * n.dot(r.d));  // IdealdielectricREFR
    const bool into = n.dot(nl) > 0;  // Ray from outside going in?
    const double nc = 1, nt = 1.5, nnt = into ? nc / nt : nt / nc,
                 ddn = r.d.dot(nl), cos2t = 1 - nnt * nnt * (1 - ddn * ddn);
    if (cos2t < 0) {  // Totalinternalrefl
        return obj.e + f.mult(radiance(reflRay, depthnew, Xi));
    }
    Vec tdir =
        (r.d * nnt - n * ((into ? 1 : -1) * (ddn * nnt + sqrt(cos2t)))).norm();
    const double a = nt - nc, b = nt + nc, R0 = a * a / (b * b),
                 c = 1 - (into ? -ddn : tdir.dot(n));
    const double Re = R0 + (1 - R0) * c * c * c * c * c, Tr = 1 - Re,
                 P = .25 + .5 * Re, RP = Re / P, TP = Tr / (1 - P);
    return obj.e +
           f.mult(depthnew > 2
                      ? (erand48(Xi) < P
                             ?  // Russian roulette
                             radiance(reflRay, depthnew, Xi) * RP
                             : radiance(Ray(x, tdir), depthnew, Xi) * TP)
                      : radiance(reflRay, depthnew, Xi) * Re +
                            radiance(Ray(x, tdir), depthnew, Xi) * Tr);
}

extern "C" {

void smallpt(const int w, const int h, const int nsamps) {
    const int samps = nsamps / 4;  // # samples
    const Ray cam(Vec(50, 52, 295.6),
                  Vec(0, -0.042612, -1).norm());  // cam pos, dir
    const Vec cx = Vec(w * .5135 / h, 0, 0), cy = (cx % cam.d).norm() * .5135;
    Vec r = Vec::zero();
    Vec *c = new Vec[w * h];
    // #pragma omp parallel for schedule(dynamic, 1) private(r)       // OpenMP
    for (int y = 0; y < h; y++) {  // Loop over image rows
        // fprintf(stderr,"\rRendering (%d spp) %5.2f%%",samps*4,100.*y/(h-1));
        unsigned short Xi[3] = {0, 0, (unsigned short)(y * y * y)};
        for (unsigned short x = 0; x < w; x++) {  // Loop cols
            const int i = (h - y - 1) * w + x;
            for (int sy = 0; sy < 2; sy++) {  // 2x2subpx
                for (int sx = 0; sx < 2;
                     sx++, r = Vec::zero()) {  // 2x2-subpx-col
                    for (int s = 0; s < samps; s++) {
                        double r1 = 2 * erand48(Xi),
                               dx = r1 < 1 ? sqrt(r1) - 1 : 1 - sqrt(2 - r1);
                        double r2 = 2 * erand48(Xi),
                               dy = r2 < 1 ? sqrt(r2) - 1 : 1 - sqrt(2 - r2);
                        Vec d = cx * (((sx + .5 + dx) / 2 + x) / w - .5) +
                                cy * (((sy + .5 + dy) / 2 + y) / h - .5) +
                                cam.d;
                        Vec dnorm = d.norm();
                        r = r +
                            radiance(Ray(cam.o + dnorm * 140, dnorm), 0, Xi) *
                                (1. / samps);
                    }  // Camera rays are pushed ^^^^^ forward to start in
                       // interior
                    c[i] = c[i] + Vec(clamp(r.x), clamp(r.y), clamp(r.z)) * .25;
                }
            }
        }
    }
    FILE *f = fopen("image-cpp.ppm", "w");  // Write image to PPM file.
    fprintf(f, "P3\n%d %d\n%d\n", w, h, 255);
    for (int i = 0; i < w * h; i++) {
        fprintf(f, "%d %d %d ", toInt(c[i].x), toInt(c[i].y), toInt(c[i].z));
    }
}
}

int main() {
    // unsigned short Xi[3] = {0, 0, (unsigned short)(200*200*200)};
    smallpt(50, 50, 256);
    return 0;
}
