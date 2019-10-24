# (Java) Core

This is the library shared amongst the java parts. 

The primary use is to reduce code-repetition and have a high-tested lib with shared api´s. With this Bib, changes should be faster doable as they don´t need to be doubled. 

The secondary use is to see and play how the maven package will fit into the CI/CD-pipeline. 
Especially things as 2 step builds and pipeline-triggers are the focus of this. 

## Run

It should run with a simple: 

`mvn clean install`

This will run the full process and install it into your local maven repository for other projects to use. This is also the way to get a new unstable-version for changes in two sub-projects.

Its considered best practice to always clean when building bibs.

## Publish

`mvn deploy`

should deploy the application. 

Due to maven logic, this will fail if there was not atleast a minor release. (e.g. from 1.0.1 to 1.0.2).

Make sure to setup your xml-settings [as from the github guide](https://help.github.com/en/github/managing-packages-with-github-package-registry/configuring-apache-maven-for-use-with-github-package-registry) **beforehand**. This means that you must have valid tokens and make changes to your local setup.

## Reading the Package

You must setup your settings similiar to above, but a read:package right is enough. 

As far as I understand, you need to have a github account in the package beta to access the packages. 

The account also needs to have read package rights. As soon as possible I want to publish "true" public, but git has not yet told me how. 
I dont want to provide a read-only token from my account as this is against the beta rules. 


## Troubleshooting

With Eclipse, I (accidently?) added my maven dependencies to my classpath. 
The classpath has additionally: 

```
<classpathentry kind="con" path="org.eclipse.m2e.MAVEN2_CLASSPATH_CONTAINER">
		<attributes>
			<attribute name="maven.pomderived" value="true"/>
		</attributes>
</classpathentry>
```

But I am not sure, whether this is actually required or not. 

## Requirements

- Java 12 
- Maven 3
- Internet