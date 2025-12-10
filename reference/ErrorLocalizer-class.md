# Base class for class locate errors based on rules and data

ErrorLocalizer can be used as a base class to implement a new error
localization algorithm. The derived class must implement two methods:
`initialize`, which is called before any error localization is done and
`locate` which operates upon data. The extra parameter `...` can used to
supply algorithmic specific parameters.
