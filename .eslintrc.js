module.exports = {
    "env": {
        "browser": true
    },
    "extends": "eslint:recommended",
    "extends": "wikimedia",
    "extends": ["plugin:prettier/recommended"],
    "parserOptions": {
        "ecmaVersion": 5
    },
    "rules": {
        "indent": [
            "error",
            "tab"
        ],
        "linebreak-style": [
            "error",
            "unix"
        ],
        "quotes": [
            "error",
            "single"
        ],
        "semi": [
            "error",
            "always"
        ]
    }
};
