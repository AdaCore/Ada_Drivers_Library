#! /usr/bin/env python3


class Int_Validation:

    def __init__(self, min_value=None, max_value=None):
        self.min_value = None if min_value is None else int(min_value)
        self.max_value = None if max_value is None else int(max_value)

    def __call__(self, value):
        try:
            int_val = int(value)
            if self.min_value is not None and int_val < self.min_value:
                return False
            elif self.max_value is not None and int_val > self.max_value:
                return False
            else:
                return True
        except ValueError:
            return False

    def kind(self):
        return "int"

    def __str__(self):
        if self.min_value is not None and self.max_value is not None:
            return "%d .. %d" % (self.min_value, self.max_value)
        elif self.min_value is not None:
            return "minimum %d" % self.min_value
        elif self.max_value is not None:
            return "maximum %d" % self.max_value
        else:
            return "any integer value"


class Float_Validation:

    def __init__(self, min_value=None, max_value=None):
        self.min_value = None if min_value is None else float(min_value)
        self.max_value = None if max_value is None else float(max_value)

    def __call__(self, value):
        try:
            float_val = float(value)
            if self.min_value is not None and float_val < self.min_value:
                return False
            elif self.max_value is not None and float_val > self.max_value:
                return False
            else:
                return True
        except ValueError:
            return False

    def kind(self):
        return "float"

    def __str__(self):
        if self.min_value is not None and self.max_value is not None:
            return "%d .. %d" % (self.min_value, self.max_value)
        elif self.min_value is not None:
            return "minimum %d" % self.min_value
        elif self.max_value is not None:
            return "maximum %d" % self.max_value
        else:
            return "any float value"


class Enum_Validation:

    def __init__(self, list_of_values):
        self.list_of_values = list_of_values

    def __call__(self, value):
        return isinstance(value, str) and value in self.list_of_values

    def kind(self):
        return "enum"

    def __str__(self):
        return ", ".join(self.list_of_values)


class String_Validation:

    def __call__(self, value):
        return isinstance(value, str)

    def kind(self):
        return "string"

    def __str__(self):
        return "any string value"


class Bool_Validation:

    def __call__(self, value):
        return isinstance(value, str) and value in {"yes": 'y',
                                                    "y": 'y',
                                                    "ye": 'y',
                                                    "True": 'y',
                                                    "no": 'n',
                                                    "n": 'n',
                                                    "False": 'n'}

    def kind(self):
        return "bool"

    def __str__(self):
        return "yes or no"
