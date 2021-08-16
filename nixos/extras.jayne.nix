hardware.acpilight.enable = true;

services.mysql.ensureDatabases = [
    "ric"
    "testing_ric"
];

services.mysql.ensureUsers = [
    {
        name = "ric";
        ensurePermissions = {
            "ric.*" = "ALL PRIVILEGES";
            "testing_ric.*" = "ALL PRIVILEGES";
        };
    }
];
