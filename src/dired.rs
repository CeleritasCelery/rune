use crate::core::{
    gc::Context,
    object::{NIL, Object, OptionalFlag, TRUE},
};
use rune_core::macros::list;
use rune_macros::defun;
use std::path::Path;

#[defun]
fn file_attributes<'ob>(filename: &str, _id_format: OptionalFlag, cx: &'ob Context) -> Object<'ob> {
    let file = Path::new(filename);
    if file.exists() { metadata_attributes(file, cx) } else { NIL }
}

#[cfg(unix)]
fn metadata_attributes<'ob>(file: &Path, cx: &'ob Context) -> Object<'ob> {
    use std::os::unix::fs::MetadataExt;
    let metadata = &file.metadata().unwrap();

    //  0. t for directory, string (name linked to) for symbolic link, or nil.
    let file_type = get_file_type(file, cx);
    //  1. Number of hardlinks to file.
    let links = metadata.nlink();
    //  2. File uid as a string or (if ID-FORMAT is integer or a string value
    //   cannot be looked up) as an integer.
    let uid = metadata.uid();
    //  3. File gid, likewise.
    let gid = metadata.gid();
    //  4. Last access time, in the style of current-time.
    //   (See a note below about access time on FAT-based filesystems.)
    let atime = metadata.atime();
    //  5. Last modification time, likewise.  This is the time of the last
    //   change to the file's contents.
    let mtime = metadata.mtime();
    //  6. Last status change time, likewise.  This is the time of last change
    //   to the file's attributes: owner and group, access mode bits, etc.
    let ctime = metadata.ctime();
    //  7. Size in bytes, as an integer.
    let size = metadata.size();
    //  8. File modes, as a string of ten letters or dashes as in ls -l.
    let mode = metadata.mode();
    //  9. An unspecified value, present only for backward compatibility.
    // 10. inode number, as a nonnegative integer.
    let inode = metadata.ino();
    // 11. Filesystem device identifier, as an integer or a cons cell of integers.
    let dev = metadata.dev();
    list![file_type, links, uid, gid, atime, mtime, ctime, size, mode, TRUE, inode, dev; cx]
}

#[cfg(windows)]
fn metadata_attributes<'ob>(file: &Path, cx: &'ob Context) -> Object<'ob> {
    use std::os::windows::fs::MetadataExt;
    let metadata = &file.metadata().unwrap();

    //  0. t for directory, string (name linked to) for symbolic link, or nil.
    let file_type = get_file_type(file, cx);
    // TODO: implement the rest of the attributes
    //  1. Number of hardlinks to file.
    //  2. File uid as a string or (if ID-FORMAT is integer or a string value
    //   cannot be looked up) as an integer.
    //  3. File gid, likewise.
    //  4. Last access time, in the style of current-time.
    //   (See a note below about access time on FAT-based filesystems.)
    //  5. Last modification time, likewise.  This is the time of the last
    //   change to the file's contents.
    //  6. Last status change time, likewise.  This is the time of last change
    //   to the file's attributes: owner and group, access mode bits, etc.
    //  7. Size in bytes, as an integer.
    //  8. File modes, as a string of ten letters or dashes as in ls -l.
    //  9. An unspecified value, present only for backward compatibility.
    // 10. inode number, as a nonnegative integer.
    // 11. Filesystem device identifier, as an integer or a cons cell of integers.
    panic!("file-attributes are not yet implemented for non-unix systems");
}

fn get_file_type<'ob>(file: &Path, cx: &'ob Context) -> Object<'ob> {
    if file.is_dir() {
        TRUE
    } else if file.is_symlink() {
        let target = file.read_link().unwrap();
        cx.add(target.to_str().unwrap())
    } else {
        NIL
    }
}
