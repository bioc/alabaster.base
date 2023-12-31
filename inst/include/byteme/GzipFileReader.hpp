#ifndef BYTEME_GZIP_FILE_READER_HPP
#define BYTEME_GZIP_FILE_READER_HPP

#include "zlib.h"
#include <stdexcept>
#include <vector>
#include <string>
#include "SelfClosingGzFile.hpp"
#include "Reader.hpp"

/**
 * @file GzipFileReader.hpp
 *
 * @brief Read a Gzip-compressed file.
 */

namespace byteme {

/**
 * @brief Read uncompressed bytes from a Gzip-compressed file.
 *
 * This is basically a wrapper around Zlib's `gzFile` with correct closing and error checking.
 */
class GzipFileReader : public Reader {
public:
    /**
     * @param path Path to the file.
     * @param buffer_size Size of the buffer to use for reading.
     */
    GzipFileReader(const char* path, size_t buffer_size = 65536) : gz(path, "rb"), buffer_(buffer_size) {}

    /**
     * @param path Path to the file.
     * @param buffer_size Size of the buffer to use for reading.
     */
    GzipFileReader(const std::string& path, size_t buffer_size = 65536) : GzipFileReader(path.c_str(), buffer_size) {}

public:
    bool load() {
        read = gzread(gz.handle, buffer_.data(), buffer_.size());
        if (read) {
            return true;
        }

        if (!gzeof(gz.handle)) { 
            int dummy;
            throw std::runtime_error(gzerror(gz.handle, &dummy));
        }

        return false;
    }

    const unsigned char* buffer() const {
        return buffer_.data();
    }

    size_t available() const {
        return read;
    }

private:
    SelfClosingGzFile gz;
    std::vector<unsigned char> buffer_;
    size_t read = 0;
};

}

#endif
