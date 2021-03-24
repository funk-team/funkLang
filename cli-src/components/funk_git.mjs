// TODO: Write tests for `parseUrl`
// Example:
// From this url: https://github.com/author_name/project_name/etc/etc/etc/etc
// this function ideally returns [true, `author_name`, `project_name`]
export const parseUrl = url => {
    maybeMatch = url.replace(new RegExp("https://github.com"), "");
    if (maybeMatch === url) {
        return [false, "the replacement failed. only github is supported atm."];
    }

    maybe_author_and_project_name = maybeMatch.match(
        new RegExp("^/[^/]*/[^/]*")
    );
    if (maybe_author_and_project_name === null) {
        return [false, "couldn't match pattern like '/author/project_name/'"];
    } else {
        // otherwise we get a chunk of data... and the first element is actually what we want
        // why have separate, clean, explicit functions for these things, amirite?
        maybe_author_and_project_name = maybe_author_and_project_name[0];
    }

    split_author_and_project = maybe_author_and_project_name.split("/");

    author = split_author_and_project[1];
    repo = split_author_and_project[2];

    return [true, author, repo];
};
// console.log(parseUrl("https://github.com/shlick_author_name/ok/"));
// (returns `[true, "shlick_author_name", "ok"])
