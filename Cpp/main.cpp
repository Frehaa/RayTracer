#include <iostream>
#include <fstream>
#include <sstream>
#include <string>
#include <vector>
#include <cmath>
#include <limits>

#include <stdlib.h>
#include <stdio.h>
#include <time.h>

struct RGB {
    unsigned char red;
    unsigned char green;
    unsigned char blue;
};

void saveBMP (const char file_name[], const int& width, const int& height, const int& dpi, const RGB data[]) {    
    FILE *f;
    const int file_header_size = 14;
    const int file_info_size = 40;
    const int header_size = file_header_size + file_info_size;
    const int color_size = 4;
    
    const int num_colors = width * height;
    const int data_size = num_colors * color_size;
    const int file_size = header_size + data_size;

    int pixels_per_meter = dpi * 39; // 39 = ???

    unsigned char bmp_file_header[file_header_size] = 
        {
            'B', 'M', 
            (unsigned char)file_size,            
            (unsigned char)file_size >> 8, 
            (unsigned char)file_size >> 16, 
            (unsigned char)file_size >> 24, 
            0, 0, 0, 0, 
            header_size, 0, 0, 0
        };

    unsigned char bmp_info_header[file_info_size] = 
        {
            40, 0, 0, 0,
            (unsigned char)width,            
            (unsigned char)width >> 8, 
            (unsigned char)width >> 16, 
            (unsigned char)width >> 24, 
            (unsigned char)height,            
            (unsigned char)height >> 8, 
            (unsigned char)height >> 16, 
            (unsigned char)height >> 24, 
            1, 0, 24, 0, 
            0, 0, 0, 0,
            0,
            (unsigned char)data_size,            
            (unsigned char)data_size >> 8, 
            (unsigned char)data_size >> 16, 
            (unsigned char)data_size >> 24, 
            (unsigned char)pixels_per_meter,            
            (unsigned char)pixels_per_meter >> 8, 
            (unsigned char)pixels_per_meter >> 16, 
            (unsigned char)pixels_per_meter >> 24, 
            (unsigned char)pixels_per_meter,            
            (unsigned char)pixels_per_meter >> 8, 
            (unsigned char)pixels_per_meter >> 16, 
            (unsigned char)pixels_per_meter >> 24, 

        };


    f = fopen(file_name, "wb");
    fwrite(bmp_file_header, 1, 14, f);
    fwrite(bmp_info_header, 1, 14, f);

    for (int i = 0; i < num_colors; i++) {
        RGB rgb = data[i];

        unsigned char color[3] = {255, 255, 255};

        fwrite(color, 1, 3, f);
    }
}

int main(int argc, char** argv) {

    const int dpi = 72;
    const int height = 2;
    const int width = 2;

    std::ofstream out("out.ppm");
    // out << "P6\n" << width << ' ' << height << "\n255" <<;

    // RGB pixels[width * height] = {};

    // out << (char)255 << (char)0 << (char)0;
    // out << (char)255 << (char)255 << (char)0;
    // out << (char)255 << (char)0 << (char)255;
    // out << (char)0 << (char)255 << (char)255;

    out << 'P' << '6' << ' ';
    out << '2' << ' ' << '2' << ' ';
    out << '2' << '5' << '5' << ' ';

    out << (char)255 << (char)0 << (char)0;
    out << (char)0 << (char)255 << (char)0;
    out << (char)0 << (char)0 << (char)255;
    out << (char)255 << (char)255 << (char)255;


    std::cout               << (char)0x50 << (char)0x36 << (char)0x0D << (char)0x0A;
    std::cout << (char)0x32 << (char)0x20 << (char)0x32 << (char)0x0D << (char)0x0A;
    std::cout << (char)0x32 << (char)0x35 << (char)0x35 << (char)0x0D << (char)0x0A;
    std::cout               << (char)0xFF << (char)0x00 << (char)0x00;
    std::cout               << (char)0x00 << (char)0xFF << (char)0x00;
    std::cout               << (char)0x00 << (char)0x00 << (char)0xFF;
    std::cout               << (char)0xFF << (char)0xFF << (char)0xFF;
    // char buffer[] = {
    //     'P', '6', '\n',
    //     '2', ' ', '2', '\n',
    //     '2', '5', '5', '\n',
    //     255, 0, 0,
    //     0, 255, 255,
    //     0, 0, 255,
    //     0, 255, 0
    // };

    // std::cout << sizeof(buffer) << ' ' << sizeof(buffer[0]);

    // FILE *file = fopen("idk.ppm", "wb");
    // fwrite(buffer, 1, sizeof(27), file);
    // fclose(file);

    // out << (char)0 << (char)250 << (char)0;
    // out << (char)0 << (char)250 << (char)0;
    // out << (char)0 << (char)250 << (char)0;
    // out << (char)0 << (char)250 << (char)0;

    // out << (char)0 << (char)0 << (char)250;
    // out << (char)0 << (char)0 << (char)250;
    // out << (char)0 << (char)0 << (char)250;
    // out << (char)0 << (char)0 << (char)250;

    // out << (char)255 << (char)0 << (char)0;
    // out << (char)255 << (char)0 << (char)0;
    // out << (char)255 << (char)0 << (char)0;
    // out << (char)255 << (char)0 << (char)0;


    int current_index = 0;
    for (int y = 0; y < height; ++y) {
        for (int x = 0; x < width; ++x) {
            current_index = y * width + x;


            // pixels[current_index].red = 255;
            // pixels[current_index].green = 0;
            // pixels[current_index].blue = 198;
        }
    }

    // saveBMP("my_first_bmp.bmp", width, height, dpi, pixels);

    return 0;
}