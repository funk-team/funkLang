import fs from "fs";
import path from "path";

export const isPathClear = dir_from => dir_to => {
    const files = fs.readdirSync(dir_from);
    // if any of the files at the top-most-level exist, then we don't need to
    // recurse at all
    for (let i = 0; i < files.length; i++) {
        currentPath = path.join(dir_to, files[i]);
        if (fs.existsSync(currentPath)) {
            return [
                false,
                currentPath + " exists already. I will not override"
            ];
        }
    }
    return true;
};

export const copyFileSync = from => to => {
    let targetFile = to;
    //if target is a directory a new file with the same name will be created
    if (fs.existsSync(to)) {
        if (fs.lstatSync(to).isDirectory()) {
            targetFile = path.join(to, path.basename(from));
        }
    }
    fs.writeFileSync(targetFile, fs.readFileSync(from));
};

export const copyDirRecursively = from => to => {
    // a bit lower in this code, we ONLY recurse if `from` is a dir
    // so only the caller can mess up calling this function
    // so the reason this is here is so the caller won't call this function on something that's not a directory
    if (!fs.lstatSync(from).isDirectory()) {
        return [false, from + " is not a directory"];
    }

    //example:
    //targetFolder = path.join("/home/user", path.basename("usr/bin/sh"));
    //targetFolder === "home/user/sh" // returns true
    let targetFolder = path.join(to, path.basename(from));
    if (!fs.existsSync(targetFolder)) {
        fs.mkdirSync(targetFolder);
    }

    files = fs.readdirSync(from);
    files.forEach(file => {
        let curSource = path.join(from, file);
        if (fs.lstatSync(curSource).isDirectory()) {
            copyDirRecursively(curSource)(targetFolder); // here we recurse
        } else {
            copyFileSync(curSource)(targetFolder);
        }
    });
    return [true, "copied recursively from " + from + " to " + to];
};
