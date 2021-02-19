jpgfiles <-
        list.files(
                path = here("C:\\Users\\Nithin\\Desktop\\Miscellaneous\\TIES-Revision"),
                recursive = TRUE,
                pattern = "\\.png",
                full.names = T
        )

#jpgfiles<-jpgfiles[c(1,2,4,3)]
magick::image_read(jpgfiles) %>%
        magick::image_montage(tile = "2", geometry = "x500+10+5") %>%
        magick::image_convert("jpg") %>%
        magick::image_write(
                format = ".jpg", path = here::here(paste("Assym","_collage.jpg",sep="")),
                quality = 100
        )
