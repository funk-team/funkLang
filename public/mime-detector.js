// https://developer.mozilla.org/en-US/docs/Web/HTTP/Basics_of_HTTP/MIME_types/Common_types
globalThis.detectMimeType = url => {
    const found = mimeMap.find(({ extension }) => url.endsWith(extension))
    return found ? found.mime : 'application/bytes'
}

const mimeMap = [
    {
        extension: '.aac',
        mime: 'audio/aac',
    },
    {
        extension: '.abw',
        mime: 'application/x-abiword',
    },
    {
        extension: '.arc',
        mime: 'application/x-freearc',
    },
    {
        extension: '.avi',
        mime: 'video/x-msvideo',
    },
    {
        extension: '.azw',
        mime: 'application/vnd.amazon.ebook',
    },
    {
        extension: '.bin',
        mime: 'application/octet-stream',
    },
    {
        extension: '.bmp',
        mime: 'image/bmp',
    },
    {
        extension: '.bz',
        mime: 'application/x-bzip',
    },
    {
        extension: '.bz2',
        mime: 'application/x-bzip2',
    },
    {
        extension: '.csh',
        mime: 'application/x-csh',
    },
    {
        extension: '.css',
        mime: 'text/css',
    },
    {
        extension: '.csv',
        mime: 'text/csv',
    },
    {
        extension: '.doc',
        mime: 'application/msword',
    },
    {
        extension: '.docx',
        mime:
            'application/vnd.openxmlformats-officedocument.wordprocessingml.document',
    },
    {
        extension: '.eot',
        mime: 'application/vnd.ms-fontobject',
    },
    {
        extension: '.epub',
        mime: 'application/epub+zip',
    },
    {
        extension: '.gz',
        mime: 'application/gzip',
    },
    {
        extension: '.gif',
        mime: 'image/gif',
    },
    {
        extension: '.html',
        mime: 'text/html',
    },
    {
        extension: '.ico',
        mime: 'image/vnd.microsoft.icon',
    },
    {
        extension: '.ics',
        mime: 'text/calendar',
    },
    {
        extension: '.jar',
        mime: 'application/java-archive',
    },
    {
        extension: '.jpg',
        mime: 'image/jpeg',
    },
    {
        extension: '.webp',
        mime: 'image/jpeg',
    },
    {
        extension: '.js',
        mime: 'application/javascript',
    },
    {
        extension: '.json',
        mime: 'application/json',
    },
    {
        extension: '.jsonld',
        mime: 'application/ld+json',
    },
    {
        extension: '.midi',
        mime: 'audio/midi audio/x-midi',
    },
    {
        extension: '.mjs',
        mime: 'text/javascript',
    },
    {
        extension: '.mp3',
        mime: 'audio/mpeg',
    },
    {
        extension: '.mpeg',
        mime: 'video/mpeg',
    },
    {
        extension: '.mpkg',
        mime: 'application/vnd.apple.installer+xml',
    },
    {
        extension: '.odp',
        mime: 'application/vnd.oasis.opendocument.presentation',
    },
    {
        extension: '.ods',
        mime: 'application/vnd.oasis.opendocument.spreadsheet',
    },
    {
        extension: '.odt',
        mime: 'application/vnd.oasis.opendocument.text',
    },
    {
        extension: '.oga',
        mime: 'audio/ogg',
    },
    {
        extension: '.ogv',
        mime: 'video/ogg',
    },
    {
        extension: '.ogx',
        mime: 'application/ogg',
    },
    {
        extension: '.opus',
        mime: 'audio/opus',
    },
    {
        extension: '.otf',
        mime: 'font/otf',
    },
    {
        extension: '.png',
        mime: 'image/png',
    },
    {
        extension: '.pdf',
        mime: 'application/pdf',
    },
    {
        extension: '.php',
        mime: 'application/x-httpd-php',
    },
    {
        extension: '.ppt',
        mime: 'application/vnd.ms-powerpoint',
    },
    {
        extension: '.pptx',
        mime:
            'application/vnd.openxmlformats-officedocument.presentationml.presentation',
    },
    {
        extension: '.rar',
        mime: 'application/vnd.rar',
    },
    {
        extension: '.rtf',
        mime: 'application/rtf',
    },
    {
        extension: '.sh',
        mime: 'application/x-sh',
    },
    {
        extension: '.svg',
        mime: 'image/svg+xml',
    },
    {
        extension: '.swf',
        mime: 'application/x-shockwave-flash',
    },
    {
        extension: '.tar',
        mime: 'application/x-tar',
    },
    {
        extension: '.tiff',
        mime: 'image/tiff',
    },
    {
        extension: '.ts',
        mime: 'video/mp2t',
    },
    {
        extension: '.ttf',
        mime: 'font/ttf',
    },
    {
        extension: '.txt',
        mime: 'text/plain',
    },
    {
        extension: '.vsd',
        mime: 'application/vnd.visio',
    },
    {
        extension: '.wav',
        mime: 'audio/wav',
    },
    {
        extension: '.weba',
        mime: 'audio/webm',
    },
    {
        extension: '.webm',
        mime: 'video/webm',
    },
    {
        extension: '.webp',
        mime: 'image/webp',
    },
    {
        extension: '.woff',
        mime: 'font/woff',
    },
    {
        extension: '.woff2',
        mime: 'font/woff2',
    },
    {
        extension: '.xhtml',
        mime: 'application/xhtml+xml',
    },
    {
        extension: '.xls',
        mime: 'application/vnd.ms-excel',
    },
    {
        extension: '.xlsx',
        mime:
            'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet',
    },
    {
        extension: '.xml',
        mime: 'application/xml',
    },
    {
        extension: '.xul',
        mime: 'application/vnd.mozilla.xul+xml',
    },
    {
        extension: '.zip',
        mime: 'application/zip',
    },
    {
        extension: '.7z',
        mime: 'application/x-7z-compressed',
    },
]
